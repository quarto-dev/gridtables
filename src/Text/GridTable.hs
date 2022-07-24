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
  , gridTable
  , GridTableParser
  , GridInfo (..)
  , emptyGridInfo
  , tableLine
  ) where

import Prelude hiding (fail, lines)
import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad (MonadPlus, forM_, void)
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.ST
import Control.Monad.State (StateT, MonadState, runStateT, gets, modify', put)
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Function (on)
import Data.Maybe (catMaybes)
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
  , gridCorners :: Set CharIndex
  , gridCells   :: Set CellTrace
  }

emptyGridInfo :: GridInfo
emptyGridInfo = GridInfo
  { gridRowSeps = Set.fromList [CharRow 1]
  , gridColSeps = Set.fromList [CharCol 1]
  , gridCorners = Set.fromList [(CharRow 1, CharCol 1)]
  , gridCells   = Set.fromList []
  }

-- | Get the next corner an remove it from the set of unparsed corners.
popCorner :: GridParser (Maybe CharIndex)
popCorner = do
  corners <- gets gridCorners
  case Set.minView corners of
    Nothing -> do
      modify' $ \gi -> gi { gridCorners = Set.empty }
      return Nothing
    Just (next, rest) -> do
      modify' $ \gi -> gi { gridCorners = rest }
      return $ Just next

scanCharGrid :: CharGrid -> GridParser (Set CellTrace)
scanCharGrid charGrid =
  popCorner >>= \case
    Nothing ->
      gets gridCells
    Just startIdx@(top, left) ->
      scanCell charGrid startIdx >>= \case
        Nothing -> scanCharGrid charGrid
        Just ((bottom, right), newrowseps, newcolseps) -> do
          let content = getLines charGrid startIdx (bottom, right)
          let cell = CellTrace content left right top bottom
          rowseps <- gets gridRowSeps
          colseps <- gets gridColSeps
          corners <- gets gridCorners
          cells   <- gets gridCells
          put $ GridInfo
            { gridRowSeps = newrowseps `Set.union` rowseps
            , gridColSeps = newcolseps `Set.union` colseps
            , gridCorners = Set.insert (top, right) $
                            Set.insert (bottom, left) corners
            , gridCells = cell `Set.insert` cells
            }
          scanCharGrid charGrid

type ScanResult = (CharIndex, Set CharRow, Set CharCol)

type RowSeps = Set CharRow
type ColSeps = Set CharCol

scanCell :: CharGrid -> CharIndex -> GridParser (Maybe ScanResult)
scanCell = scanRight

scanRight :: CharGrid -> CharIndex -> GridParser (Maybe ScanResult)
scanRight charGrid start@(top, left) = do
  loop Set.empty (left + 1)
  where
    loop :: ColSeps -> CharCol -> GridParser (Maybe ScanResult)
    loop colseps j
      | not (bounds charGrid `inRange` (top, j)) = return Nothing
      | otherwise = case charGrid ! (top, j) of
          Just '-' -> loop colseps (j + 1)
          Just '+' -> do
            let colseps' = Set.insert j colseps
            case scanDown charGrid start j of
              Nothing -> loop colseps' (j + 1)
              Just (end, rowseps, newcolseps) -> pure $
                Just (end, rowseps, colseps' `Set.union` newcolseps)
          _ -> return Nothing

scanDown :: CharGrid -> CharIndex -> CharCol
         -> Maybe ScanResult
scanDown charGrid start@(top, _left) right = do
  loop Set.empty (top + 1)
  where
    loop :: RowSeps -> CharRow -> Maybe ScanResult
    loop rowseps i =
      if not (bounds charGrid `inRange` (i, right))
      then Nothing
      else case charGrid ! (i, right) of
             Just '+' ->
               let rowseps' = Set.insert i rowseps
               in case scanLeft charGrid start (i, right) of
                    Nothing -> loop rowseps' (i + 1)
                    Just (newrowseps, colseps) ->
                      Just ( (i, right)
                           , rowseps' `Set.union` newrowseps
                           , colseps
                           )
             Just '|' -> loop rowseps (i + 1)
             _ -> -- all but the final column must be terminated
               if right == snd (snd (bounds charGrid))
               then loop rowseps (i + 1)
               else Nothing

scanLeft :: CharGrid -> CharIndex -> CharIndex
         -> Maybe (RowSeps, ColSeps)
scanLeft charGrid start@(_top,left) end@(bottom, right) =
  let  go :: CharCol -> Maybe ColSeps -> Maybe ColSeps
       go _ Nothing = Nothing
       go j (Just colseps) = case charGrid ! (bottom, j) of
                               Just '+' -> Just (Set.insert j colseps)
                               Just '-' -> Just colseps
                               _        -> Nothing

  in if charGrid ! (bottom, left) /= Just '+'
     then Nothing
     else
       case foldr go (Just Set.empty) [(right - 1), right - 2 .. (left + 1)] of
         Nothing      -> Nothing
         Just colseps ->
           case scanUp charGrid start end of
             Just rowseps -> Just (rowseps, colseps)
             Nothing      -> Nothing
 where

scanUp :: CharGrid -> CharIndex -> CharIndex
       -> Maybe RowSeps
scanUp charGrid (top, left) (bottom, _right) =
  let go :: CharRow -> Maybe RowSeps -> Maybe RowSeps
      go _ Nothing = Nothing
      go i (Just rowseps) = case charGrid ! (i, left) of
                              Just '+' -> Just (Set.insert i rowseps)
                              Just '|' -> Just rowseps
                              _        -> Nothing
  in foldr go (Just Set.empty) [bottom - 1, bottom - 2 .. top + 1]

-- | Get lines of a cell
getLines :: CharGrid -> CharIndex -> CharIndex -> [Text]
getLines charGrid (top, left) (bottom, right) =
  let rows = [top + 1 .. bottom - 1]
      columns = [left + 1 .. right - 1]
  in map (\ir -> T.pack . catMaybes $
                 map (\ic -> charGrid ! (ir, ic)) columns)
         rows


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
  lines <- many1 tableLine
  skipMany space *> (eof <|> void newline) -- blank line
  let charGrid = toCharGrid lines
  (cells, gridInfo) <- case runStateT (runGridParser (scanCharGrid charGrid))
                                      emptyGridInfo of
    Failure err -> fail $ show err
    Success cs  -> return cs
  return $ tableFromScanningData gridInfo cells

type CharGrid = Array (CharRow, CharCol) (Maybe Char)

type CharIndex = (CharRow, CharCol)

toCharGrid :: [Text] -> CharGrid
toCharGrid lines =
  let chars = foldr (\t m -> max m (T.length t)) 0 lines
      gbounds = ( (CharRow 1, CharCol 1)
                , (CharRow (length lines), CharCol chars)
                )
      extendedLines =   map (\line -> take chars (line ++ repeat Nothing))
                      . map (map Just . T.unpack)
                      $ lines
  in listArray gbounds (mconcat extendedLines)

-- | Parses a line that's part of a table. The line must start with
-- either a plus @+@ or a pipe @|@.
tableLine :: Stream s m Char
          => ParsecT s u m Text
tableLine = try $ do
  let borderChar = char '+' <|> char '|'
  firstChar <- borderChar
  rest <- manyTill (noneOf "\n\r") newline
  return $ firstChar `T.cons` T.pack rest

-- | Character row
newtype CharRow = CharRow Int
  deriving stock (Eq, Show)
  deriving newtype (Enum, Ix, Num, Ord)

-- | Character column
newtype CharCol = CharCol Int
  deriving stock (Eq, Show)
  deriving newtype (Enum, Ix, Num, Ord)

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
              writeArray tblgrid contIdx $
                FilledCell (ContinuationCell idx)
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
