{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{- |
Module      : Text.GridTable
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Trace cells of a grid table.
-}

module Text.GridTable.Trace
  ( traceLines
  , TraceInfo (..)
  , initialTraceInfo
  , tableFromTraceInfo
  ) where

import Prelude hiding (lines)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Text.DocLayout (charWidth)
import Text.GridTable.ArrayTable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Traces out the cells in the given lines and converts them to a
-- table containing the bare cell lines.
traceLines :: [Text] -> Maybe (GridTable [Text])
traceLines lines =
  let charGrid = toCharGrid lines
      bodySeps = findSeparators charGrid
      charGrid' = convertToNormalLines bodySeps charGrid
      traceInfo = traceCharGrid charGrid' initialTraceInfo
  in if Set.null (gridCells traceInfo)
     then fail "no cells"
     else return $ tableFromTraceInfo traceInfo bodySeps

-- | Type used to represent the 2D layout of table characters
type CharGrid = Array (CharRow, CharCol) (Maybe Char)

-- | Index of a half-width character in the character-wise
-- representation.
type CharIndex = (CharRow, CharCol)

-- | Character row
newtype CharRow = CharRow Int
  deriving stock (Eq, Show)
  deriving newtype (Enum, Ix, Num, Ord)

-- | Character column
newtype CharCol = CharCol { fromCharCol :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Enum, Ix, Num, Ord)

-- | Converts a list of lines into an char array.
toCharGrid :: [Text] -> CharGrid
toCharGrid lines =
  let chars = foldr (\t m -> max m (T.length t)) 0 lines
      gbounds = ( (CharRow 1, CharCol 1)
                , (CharRow (length lines), CharCol chars)
                )
      charList c = case charWidth c of
                     2 -> [Just c, Nothing]
                     _ -> [Just c]
      extendedLines = map ((\line -> take chars (line ++ repeat Nothing))
                            . concatMap charList . T.unpack)
                          lines
  in listArray gbounds (mconcat extendedLines)

-- | Finds the row indices of all separator lines, i.e., lines that
-- contain only @+@ and @=@ characters.
findSeparators :: CharGrid -> [CharRow]
findSeparators charGrid = foldr go [] rowIdxs
  where
    gbounds = bounds charGrid
    rowIdxs = [fst (fst gbounds) .. fst (snd gbounds)]
    colIdxs = [snd (fst gbounds) .. snd (snd gbounds)]
    isSepChar = (Just True ==) . fmap (`elem` ("+=" :: String))
    go i seps = if all isSepChar [ charGrid ! (i, j) | j <- colIdxs ]
                then i : seps
                else seps

-- | Returns new character grid in which the given lines have been
-- converted to normal cell-separating lines.
convertToNormalLines :: [CharRow] -> CharGrid -> CharGrid
convertToNormalLines sepLines charGrid = runSTArray $ do
  mutGrid <- thaw charGrid
  let gbounds = bounds charGrid
      cols = [snd (fst gbounds) .. snd (snd gbounds)]
  forM_ sepLines $ \rowidx -> do
    forM_ cols $ \colidx -> do
      let idx = (rowidx, colidx)
      c <- readArray mutGrid idx
      case c of
        Just '=' -> writeArray mutGrid idx (Just '-')
        _        -> pure ()
  return mutGrid

-- | Info on the grid. Used to keep track of information collected while
-- tracing a character grid. The set of cells is used as a kind of queue
-- during parsing, while the other data is required to assemble the
-- final table.
data TraceInfo = TraceInfo
  { gridRowSeps :: Set CharRow
  , gridColSeps :: Set CharCol
  , gridCorners :: Set CharIndex
  , gridCells   :: Set CellTrace
  }

-- | Initial tracing info.
initialTraceInfo :: TraceInfo
initialTraceInfo = TraceInfo
  { gridRowSeps = Set.fromList [CharRow 1]
  , gridColSeps = Set.fromList [CharCol 1]
  , gridCorners = Set.fromList [(CharRow 1, CharCol 1)]
  , gridCells   = Set.fromList []
  }

traceCharGrid :: CharGrid -> TraceInfo -> TraceInfo
traceCharGrid charGrid traceInfo =
  -- Get the next corner an remove it from the set of unparsed corners.
  case Set.minView (gridCorners traceInfo) of
    Nothing -> traceInfo
    Just (startIdx@(top, left), corners) ->
      case traceCell charGrid startIdx of
        Nothing ->
          -- Corner is not a top-left corner of another cell. Continue
          -- with the remaining corners.
          traceCharGrid charGrid traceInfo { gridCorners = corners }
        Just ((bottom, right), newrowseps, newcolseps) -> do
          let content = getLines charGrid startIdx (bottom, right)
          let cell = CellTrace content left right top bottom
          let rowseps = gridRowSeps traceInfo
          let colseps = gridColSeps traceInfo
          let cells   = gridCells traceInfo
          traceCharGrid charGrid $ TraceInfo
            { gridRowSeps = newrowseps `Set.union` rowseps
            , gridColSeps = newcolseps `Set.union` colseps
            , gridCorners = Set.insert (top, right) $
                            Set.insert (bottom, left) corners
            , gridCells = cell `Set.insert` cells
            }

type ScanResult = (CharIndex, Set CharRow, Set CharCol)

type RowSeps = Set CharRow
type ColSeps = Set CharCol

traceCell :: CharGrid -> CharIndex -> Maybe ScanResult
traceCell = scanRight

scanRight :: CharGrid -> CharIndex -> Maybe ScanResult
scanRight charGrid start@(top, left) = do
  loop Set.empty (left + 1)
  where
    loop :: ColSeps -> CharCol -> Maybe ScanResult
    loop colseps j
      | not (bounds charGrid `inRange` (top, j)) = Nothing
      | otherwise = case charGrid ! (top, j) of
          Just '-' -> loop colseps (j + 1)
          Just '+' ->
            let colseps' = Set.insert j colseps
            in case scanDown charGrid start j of
                 Nothing -> loop colseps' (j + 1)
                 Just (end, rowseps, newcolseps) -> pure
                   ( end
                   , rowseps
                   , colseps' `Set.union` newcolseps
                   )
          _ -> Nothing

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
  let rowIdxs = [top + 1 .. bottom - 1]
      colIdxs = [left + 1 .. right - 1]
  in map (\ir -> T.pack $ mapMaybe (\ic -> charGrid ! (ir, ic)) colIdxs)
         rowIdxs

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
tableFromTraceInfo :: TraceInfo -> [CharRow] -> GridTable [Text]
tableFromTraceInfo traceInfo bodySeps =
  let rowseps = Set.toAscList $ gridRowSeps traceInfo
      colseps = Set.toAscList $ gridColSeps traceInfo
      rowindex = Map.fromList $ zip rowseps [1..]
      colindex = Map.fromList $ zip colseps [1..]
      nrows = Map.size rowindex - 1
      ncols = Map.size colindex - 1
      gbounds = ( (RowIndex 1, ColIndex 1)
                , (RowIndex nrows, ColIndex ncols)
                )
      colwidths = [ b - a - 1 | (b, a) <- zip (tail colseps) colseps ]
      mutableTableGrid :: ST s (STArray s CellIndex (GridCell [Text]))
      mutableTableGrid = do
        tblgrid <- newArray gbounds FreeCell
        forM_ (Set.toAscList $ gridCells traceInfo) $
          \(CellTrace content left right top bottom) -> do
            let cellPos = do
                  rnum <- Map.lookup top rowindex
                  cnum <- Map.lookup left colindex
                  rs   <- RowSpan . fromRowIndex . subtract rnum <$>
                          Map.lookup bottom rowindex
                  cs   <- ColSpan . fromColIndex . subtract cnum <$>
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
        let fromBuilderCell :: BuilderCell -> GridCell [Text]
            fromBuilderCell = \case
              FilledCell c -> c
              FreeCell     -> error "Found an unassigned cell."
        getAssocs tblgrid >>= (\kvs -> forM_ kvs $ \(idx, bc) ->
          case bc of
            FreeCell -> error $ "unassigned: " ++ show idx
            _ -> pure ())
        mapArray fromBuilderCell tblgrid
  in GridTable
     { gridTableArray = runSTArray mutableTableGrid
     , gridTableHead = subtract 1 <$>
                       foldr ((<|>) . (`Map.lookup` rowindex)) Nothing bodySeps
     , gridTableColWidths = map fromCharCol colwidths
     }

continuationIndices :: (RowIndex, ColIndex)
                    -> RowSpan -> ColSpan
                    -> [CellIndex]
continuationIndices (RowIndex ridx, ColIndex cidx) rowspan colspan =
  let (RowSpan rs) = rowspan
      (ColSpan cs) = colspan
  in [ (RowIndex r, ColIndex c) | r <- [ridx..(ridx + rs - 1)]
                                , c <- [cidx..(cidx + cs - 1)]
                                , (r, c) /= (ridx, cidx)]

data BuilderCell
  = FilledCell (GridCell [Text])
  | FreeCell
