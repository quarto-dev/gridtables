{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{- |
Module      : Text.GridTable
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Parse reStructuredText-style grid tables.
-}

module Text.GridTable
  ( GridTable (..)
  , Cell (..)
  , Line (..)
  , gridTable
  , tableLines
  , GridTableParser
  , GridInfo (..)
  , emptyGridInfo
  ) where

import Prelude hiding (lines)
import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad (MonadPlus, void)
import Control.Monad.State (StateT, evalStateT)
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.Monoid (Alt (Alt, getAlt))
import Data.Set (Set)
import Data.Text (Text)
import Text.Parsec hiding ((<|>), Line, choice)
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Raw grid table.
data GridTable = GridTable [Cell]
  deriving stock (Eq, Show)

-- | Raw grid table cell
newtype Cell = Cell { cellLines :: [Text] }
  deriving stock (Eq, Ord, Show)

-- | Info on the grid. Used as parser state.
data GridInfo = GridInfo
  { rowSeps :: Set CharRow
  , colSeps :: Set CharCol
  }

emptyGridInfo :: GridInfo
emptyGridInfo = GridInfo
  { rowSeps = Set.empty
  , colSeps = Set.empty
  }

-- | Grid table parsing error.
data GridError = GridError String
  deriving stock (Eq, Ord, Show)

-- | Parse result.
data Result a
  = Success a
  | Failure GridError
  deriving stock (Eq, Functor, Ord, Show)

instance Applicative Result where
  pure = Success
  Failure e <*> _ = Failure e
  Success f <*> a = fmap f a

instance Monad Result where
  Failure e >>= _ = Failure e
  Success a >>= k = k a

instance MonadFail Result where
  fail = Failure . GridError

instance Alternative Result where
  empty = Failure (GridError "empty")
  (Failure _) <|> x = x
  (Success s) <|> _ = Success s

instance MonadPlus Result


-- | Parser for raw grid tables.
newtype GridParser a = GridParser { runGridParser :: StateT GridInfo Result a }
  deriving newtype (Alternative, Functor, Applicative, Monad, MonadFail)

-- instance MonadFail GridParser where
--   fail = pure . fail

-- | Parser type
type GridTableParser m a = ParsecT Text GridInfo m a

-- | Parses a grid table.
gridTable :: Monad m => GridTableParser m GridTable
gridTable = do
  lines <- tableLines
  skipMany space *> (eof <|> void newline) -- blank line
  cells <- case flip evalStateT emptyGridInfo . runGridParser $ loop Set.empty lines of
    Failure err -> fail $ show err
    Success cs  -> return cs
  return $ tableFromScanningData cells
 where
  loop cells lines = traceCell lines >>= \case
    Just (c, lines') -> loop (Set.insert c cells) lines'
    Nothing          -> return cells

-- | Line of text that make up a table.
--
-- Instances for 'Show' and 'Eq' are derived for testing purposes.
data Line = Line
  { lineNum     :: CharRow
  , lineCharPos :: CharCol
  , lineText    :: Text
  }
  deriving stock (Eq, Show)

-- | Parses the lines of a grid table.
tableLines :: Stream s m Char
           => ParsecT s u m [Line]
tableLines = zipWith mkLine [1..] <$> many tableLine
  where mkLine lnum txt = Line
          { lineNum = lnum
          , lineCharPos = 0
          , lineText = txt
          }

-- | Parses a line that's part of a table. The line must start with
-- either a plus @+@ or a pipe @|@.
tableLine :: Stream s m Char
          => ParsecT s u m Text
tableLine = try $ do
  firstChar <- char '+' <|> char '|'
  rest <- manyTill anyChar (try $ char firstChar *> newline)
  return $ firstChar `T.cons` T.pack rest `T.snoc` firstChar

-- | Character column, adjusted for wide characters
newtype CharCol = CharCol Int
  deriving stock (Eq, Show)
  deriving newtype (Enum, Num, Ord)

-- | Character row
newtype CharRow = CharRow Int
  deriving stock (Eq, Show)
  deriving newtype (Enum, Num, Ord)

data CellTrace = CellTrace
  { cellTraceContent :: Cell
  , cellTraceLeft    :: CharCol
  , cellTraceTop     :: CharRow
  }
  deriving stock (Eq, Show)

instance Ord CellTrace where
  x `compare` y =
    case (compare `on` cellTraceTop) x y of
      EQ -> (compare `on` cellTraceLeft) x y
      o  -> o

-- | Trace the next rectangular cell.
traceCell :: [Line]
          -> GridParser (Maybe (CellTrace, [Line]))
traceCell [] = return Nothing
traceCell lines@(line:_) = do
  (cell, unparsedLines) <- scanTopBorder lines
  let isParsedLine line' = case T.uncons (lineText line') of
        Just (_, "") -> True
        _            -> False
  let cellTrace = CellTrace
        { cellTraceContent = cell
        , cellTraceLeft    = lineCharPos line
        , cellTraceTop     = lineNum line
        }

  return . Just $
    ( cellTrace
    , dropWhile isParsedLine unparsedLines
    )

choice :: [GridParser a] -> GridParser a
choice = getAlt . mconcat . map Alt

scanTopBorder :: [Line]
              -> GridParser (Cell, [Line])  -- ^ Traced cell and remaining lines
scanTopBorder [] = fail "Cannot scan top border: no lines left"
scanTopBorder (line:lines) = choice $
  (map scanTop' . drop 1 . T.breakOnAll "+" $ lineText line)
  where
    scanTop' (before, topUnparsed) = do
      let nChars = T.length before
      nLines      <- scanLeftBorder lines
      (cellContent, unparsedRight) <-
        scanRightBorder nChars nLines (take nLines lines)
      unparsedBelow <-
        scanBottomBorder nChars (drop nLines lines)
      return ( Cell cellContent
             , line { lineText = topUnparsed
                    , lineCharPos = lineCharPos line + CharCol (T.length before)
                    }
               : unparsedRight ++ unparsedBelow)

scanLeftBorder :: [Line]
               -> GridParser Int
scanLeftBorder = scanLeftBorder' 0
  where scanLeftBorder' numlines = \case
          [] -> fail "unterminated left border"
          (l:ls) -> case T.uncons (lineText l) of
                      Just ('|', _) -> scanLeftBorder' (numlines + 1) ls
                      Just ('+', _) -> return numlines
                      _ -> fail "unterminated left border"

scanRightBorder :: Int     -- ^ Number of chars in column
                -> Int     -- ^ Number lines
                -> [Line]  -- ^ Table text lines
                -> GridParser ([Text], [Line])
scanRightBorder nChars _nLines lines = do
  foldrM scanRightBorder' ([], []) lines
  where
    scanRightBorder' line (cellLinesAcc, unparsed) =
      case T.uncons (lineText line) of
        Nothing -> fail "empty line"
        Just ('|', lineText') ->
          let (cellLine, rest) = T.splitAt (nChars - 1) lineText'
          in case T.uncons rest of
               Just (c, _) ->
                 if c == '+' || c == '|'
                 then do
                   let line' = line { lineText = rest
                                    , lineCharPos = lineCharPos line +
                                                    CharCol (T.length cellLine)
                                    }
                   return ( cellLine : cellLinesAcc
                          , line' : unparsed)
                 else fail $ "not a border char: " ++ [c]
               Nothing -> fail "unterminated cell"
        Just (c, _) -> fail $ "unexpected char " ++ [c]

scanBottomBorder :: Int               -- ^ Number of chars in cell lines
                 -> [Line]
                 -> GridParser [Line]
scanBottomBorder _nchars [] = fail "missing bottom border"
scanBottomBorder nchars (line : rest) = do
  let (_, unparsed) = T.splitAt nchars (lineText line)
  return (line { lineText = unparsed } : rest)

-- | Create a final grid table from line scanning data.
tableFromScanningData :: Set CellTrace -> GridTable
tableFromScanningData cells =
  GridTable (map cellTraceContent $ Set.toAscList cells)
