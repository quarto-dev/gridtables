{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
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
import Control.Monad (void)
import Data.Foldable (foldrM)
import Data.Set (Set)
import Data.Text (Text)
import Text.Parsec hiding (Line, choice)
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Raw grid table.
data GridTable = GridTable [Cell]
  deriving stock (Eq, Show)

-- | Raw grid table cell
newtype Cell = Cell { cellLines :: [Text] }
  deriving stock (Eq, Show)

-- | Info on the grid. Used as parser state.
data GridInfo = GridInfo
  { rowSeps :: Set CharRow
  , colSeps :: Set CharCol
  }

-- | Grid table parsing error.
data GridError = GridError String
  deriving stock (Eq, Ord, Show)

-- | Parser for raw grid tables.
data GridParser a
  = Success a
  | Failure GridError
  deriving stock (Eq, Functor, Ord, Show)

instance Applicative GridParser where
  pure = Success
  Failure e <*> _ = Failure e
  Success f <*> a = fmap f a

instance Monad GridParser where
  Failure e >>= _ = Failure e
  Success a >>= k = k a

instance MonadFail GridParser where
  fail = Failure . GridError

emptyGridInfo :: GridInfo
emptyGridInfo = GridInfo
  { rowSeps = Set.empty
  , colSeps = Set.empty
  }

-- | Parser type
type GridTableParser m a = ParsecT Text GridInfo m a

-- | Parses a grid table.
gridTable :: Monad m => GridTableParser m GridTable
gridTable = do
  lines <- tableLines
  skipMany space *> (eof <|> void newline) -- blank line
  cells <- case loop [] lines of
    Failure err -> fail $ show err
    Success cs  -> return cs
  return $ GridTable (reverse cells)
 where
  loop cells lines = traceCell lines >>= \case
    (NoTrace, _) ->
      return cells
    (CellTrace cell _right _bottom, lines') ->
      loop (cell:cells) lines'
      -- error ("here we go again" ++ show lines')

-- | Line of text that make up a table.
--
-- Instances for 'Show' and 'Eq' are derived for testing purposes.
data Line = Line
  { lineNum  :: Int
  , lineText :: Text
  }
  deriving stock (Eq, Show)

-- | Parses the lines of a grid table.
tableLines :: Stream s m Char
           => ParsecT s u m [Line]
tableLines = zipWith Line [1..] <$> many tableLine

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

-- | Character row
newtype CharRow = CharRow Int


data CellTrace = CellTrace Cell CharCol CharRow
               | NoTrace

-- | Trace the next rectangular cell.
traceCell :: [Line]
          -> GridParser (CellTrace, [Line])
traceCell [] = return (NoTrace, [])
traceCell lines = do
  (cell, unparsedLines) <- scanTopBorder lines
  let isParsedLine line = case T.uncons (lineText line) of
        Just (_, "") -> True
        _            -> False
  return $ ( CellTrace cell (CharCol $ length (cellLines cell)) (CharRow 0)
           , dropWhile isParsedLine unparsedLines
           )

choice :: [GridParser a] -> GridParser a
choice = \case
  []            -> fail "no choice left"
  [Failure err] -> Failure err
  x:xs          -> case x of
                     Failure _ -> choice xs
                     y         -> y

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
             , line { lineText = topUnparsed }:unparsedRight ++ unparsedBelow)

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
                   let line' = line { lineText = rest }
                   return $ (cellLine : cellLinesAcc, line':unparsed)
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
