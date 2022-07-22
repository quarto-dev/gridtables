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
  , RowIndex (..)
  , ColIndex (..)
  , GridCell (..)
  , Cell (..)
  , Line (..)
  , gridTable
  , tableLines
  , GridTableParser
  , GridInfo (..)
  , emptyGridInfo
  ) where

import Prelude hiding (fail, lines)
import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad (MonadPlus, forM_, void)
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.ST
import Control.Monad.State (StateT, MonadState, runStateT, modify')
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.Monoid (Alt (Alt, getAlt))
import Data.Set (Set)
import Data.Text (Text)
import Text.Parsec hiding ((<|>), Line, choice)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Grid table: Cells are placed on a grid.
newtype GridTable = GridTable
  { gridTableArray :: Array CellIndex GridCell
  }
  deriving stock (Eq, Show)

-- | Raw grid table cell
newtype Cell = Cell { cellLines :: [Text] }
  deriving stock (Eq, Ord, Show)

-- | Info on the grid. Used as parser state.
data GridInfo = GridInfo
  { gridRowSeps :: Set CharRow
  , gridColSeps :: Set CharCol
  }

emptyGridInfo :: GridInfo
emptyGridInfo = GridInfo
  { gridRowSeps = Set.fromList [CharRow 1]
  , gridColSeps = Set.fromList [CharCol 1]
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
newtype GridParser a = GridParser
  { runGridParser :: StateT GridInfo Result a
  }
  deriving newtype (
    Alternative,
    Applicative,
    Functor,
    Monad,
    MonadFail,
    MonadState GridInfo
  )

-- | Parser type
type GridTableParser m a = ParsecT Text GridInfo m a

-- | Parses a grid table.
gridTable :: Monad m => GridTableParser m GridTable
gridTable = do
  lines <- tableLines
  skipMany space *> (eof <|> void newline) -- blank line
  (cells, gridInfo) <- case runStateT (runGridParser (loop Set.empty lines))
                                      emptyGridInfo of
    Failure err -> fail $ show err
    Success cs  -> return cs
  return $ tableFromScanningData gridInfo cells
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
          , lineCharPos = 1
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
  { cellTraceContent :: [Text]
  , cellTraceLeft    :: CharCol
  , cellTraceRight   :: CharCol
  , cellTraceTop     :: CharRow
  , cellTraceBottom  :: CharRow
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
  (cell, width, unparsedLines) <- scanTopBorder lines
  let isParsedLine line' = case T.uncons (lineText line') of
        Just (_, "") -> True
        _            -> False
      top = lineNum line
  let cellTrace = CellTrace
        { cellTraceContent = cellLines cell
        , cellTraceLeft    = lineCharPos line
        , cellTraceRight   = lineCharPos line + CharCol width
        , cellTraceTop     = lineNum line
        , cellTraceBottom  = top + CharRow (length $ cellLines cell) + 1
        }

  return . Just $
    ( cellTrace
    , dropWhile isParsedLine unparsedLines
    )

choice :: [GridParser a] -> GridParser a
choice = getAlt . mconcat . map Alt

scanTopBorder :: [Line]
              -> GridParser (Cell, Int, [Line])  -- ^ Traced cell, its width,
                                                 -- and remaining lines
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
      modify' $ \gi -> gi { gridColSeps =
                            Set.insert (CharCol nChars + lineCharPos line)
                                       (gridColSeps gi)
                          }
      return ( Cell cellContent
             , nChars
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
  let bottom = lineNum line
  modify' $ \gi -> gi { gridRowSeps = Set.insert bottom $ gridRowSeps gi }
  return (line { lineText = unparsed } : rest)

-- | Create a final grid table from line scanning data.
tableFromScanningData :: GridInfo -> Set CellTrace -> GridTable
tableFromScanningData gridInfo cells =
  let rowseps = Set.toAscList $ gridRowSeps gridInfo
      colseps = Set.toAscList $ gridColSeps gridInfo
      rowindex = Map.fromList $ zip rowseps [1..]
      colindex = Map.fromList $ zip colseps [1..]
      nrows = Map.size rowindex - 1
      ncols = Map.size colindex - 1
      gbounds = ( (RowIndex 1, ColIndex 1)
                , (RowIndex nrows, ColIndex ncols)
                )
      mutableTableGrid :: ST s (STArray s CellIndex GridCell)
      mutableTableGrid = do
        tblgrid <- newArray gbounds FreeCell
        forM_ (Set.toAscList cells) $
          \(CellTrace content left right top bottom) -> do
            let cellPos = do
                  rnum <- Map.lookup top rowindex
                  cnum <- Map.lookup left colindex
                  rs   <- RowSpan . fromRowIndex . (subtract rnum) <$>
                          Map.lookup bottom rowindex
                  -- let rs = 1
                  cs   <- ColSpan . fromColIndex . (subtract cnum) <$>
                          Map.lookup right colindex
                  pure ((rnum, cnum), rs, cs)
            let (idx, rowspan, colspan) = case cellPos of
                  Just cp -> cp
                  Nothing -> error "A cell or row index was not found"
            writeArray tblgrid idx . FilledCell $
              ContentCell rowspan colspan content
            forM_ (continuationIndices idx rowspan colspan) $ \contIdx -> do
              -- FIXME: ensure that the cell has not been filled yet
              writeArray tblgrid contIdx . FilledCell $ ContinuationCell contIdx
            -- Swap BuilderCells with normal GridCells.
        let fromBuilderCell :: BuilderCell -> GridCell
            fromBuilderCell = \case
              FilledCell c -> c
              FreeCell     -> error $ "Found an unassigned cell."
        getAssocs tblgrid >>= (\kvs -> forM_ kvs $ \(idx, bc) ->
          case bc of
            FreeCell -> error $ "unassigned: " ++ show idx
            _ -> pure ())
        mapArray fromBuilderCell tblgrid
  in GridTable $ runSTArray mutableTableGrid

continuationIndices :: (RowIndex, ColIndex)
                    -> RowSpan -> ColSpan
                    -> [CellIndex]
continuationIndices (RowIndex ridx, ColIndex cidx) rowspan colspan =
  let (RowSpan rs) = rowspan
      (ColSpan cs) = colspan
  in [ (RowIndex r, ColIndex c) | r <- [ridx..(ridx + rs - 1)]
                                , c <- [cidx..(cidx + cs - 1)]
                                , (r, c) /= (ridx, cidx)]

-- | Row index in a table array.
newtype RowIndex = RowIndex { fromRowIndex :: Int }
  deriving stock (Eq, Ix, Ord)
  deriving newtype (Enum, Num, Show)

-- | Column index in a table array.
newtype ColIndex = ColIndex { fromColIndex :: Int }
  deriving stock (Eq, Ix, Ord)
  deriving newtype (Enum, Num, Show)

-- | Index to a cell in a table part.
type CellIndex = (RowIndex, ColIndex)

-- | A grid cell contains either a real table cell, or is the
-- continuation of a column or row-spanning cell. In the latter case,
-- the index of the continued cell is provided.
data GridCell
  = ContentCell RowSpan ColSpan [Text]
  | ContinuationCell CellIndex
  deriving stock (Eq, Show)

data BuilderCell
  = FilledCell GridCell
  | FreeCell

-- | The number of rows spanned by a cell.
newtype RowSpan = RowSpan Int
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Read, Show)

-- | The number of columns spanned by a cell.
newtype ColSpan = ColSpan Int
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Read, Show)
