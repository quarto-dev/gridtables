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
import Data.Maybe (fromMaybe, mapMaybe)
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
      specs1   = colSpecsInLine '-' charGrid 1
      partSeps = findSeparators charGrid
      charGrid' = convertToNormalLines (1:map partSepLine partSeps) charGrid
      traceInfo = traceCharGrid charGrid' initialTraceInfo
  in if Set.null (gridCells traceInfo)
     then fail "no cells"
     else return $ tableFromTraceInfo traceInfo partSeps specs1

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

data PartSeparator = PartSeparator
  { partSepLine    :: CharRow
  , partSepColSpec :: [ColSpec]
  }

data ColSpec = ColSpec
  { colStart :: CharCol
  , colEnd   :: CharCol
  , colAlign :: Alignment
  }

-- | Finds the row indices of all separator lines, i.e., lines that
-- contain only @+@ and @=@ characters.
findSeparators :: CharGrid -> [PartSeparator]
findSeparators charGrid = foldr go [] rowIdxs
  where
    gbounds = bounds charGrid
    rowIdxs = [fst (fst gbounds) .. fst (snd gbounds)]
    go i seps = case colSpecsInLine '=' charGrid i of
                  Nothing -> seps
                  Just colspecs -> PartSeparator i colspecs : seps

colSpecsInLine :: Char -> CharGrid -> CharRow -> Maybe [ColSpec]
colSpecsInLine c charGrid i =
  case charGrid ! (i, firstCol) of
    Just '+' -> loop [] (firstCol + 1)
    _        -> Nothing
  where
    loop acc j = case colSpecAt j of
                   Nothing      -> Nothing
                   Just Nothing -> Just $ reverse acc
                   Just (Just colspec) ->
                     loop (colspec:acc) (colEnd colspec + 1)
    gbounds = bounds charGrid
    firstCol = snd (fst gbounds)
    lastCol = snd (snd gbounds)
    colSpecAt :: CharCol -> Maybe (Maybe ColSpec)
    colSpecAt j
      | j >= lastCol = Just Nothing
      | otherwise = case findEnd (j + 1) of
          Nothing               -> Nothing
          Just (end, rightMark) ->
            let leftMark = charGrid ! (i, j) == Just ':'
                align = case (leftMark, rightMark) of
                  (False , False) -> AlignDefault
                  (True  , False) -> AlignLeft
                  (False , True ) -> AlignRight
                  (True  , True ) -> AlignCenter
                colspec = ColSpec
                  { colStart = j
                  , colEnd = end
                  , colAlign = align
                  }
            in pure (pure colspec)
    findEnd j = case charGrid ! (i, j) of
      Just '+' -> pure (j, False)
      Just ':' -> if charGrid ! (i, j + 1) == Just '+'
                  then pure (j + 1, True)
                  else Nothing
      Just c'
        | c' == c -> findEnd (j + 1)
      _           -> Nothing

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
      -- convert `=` to `-` and remove alignment markers
      case c of
        Just '=' -> writeArray mutGrid idx (Just '-')
        Just ':' -> writeArray mutGrid idx (Just '-')
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
tableFromTraceInfo :: TraceInfo
                   -> [PartSeparator]
                   -> Maybe [ColSpec]
                   -> GridTable [Text]
tableFromTraceInfo traceInfo partSeps colSpecsFirstLine =
  let rowseps = Set.toAscList $ gridRowSeps traceInfo
      colseps = Set.toAscList $ gridColSeps traceInfo
      rowindex = Map.fromList $ zip rowseps [1..]
      colindex = Map.fromList $ zip colseps [1..]
      colwidths = [ b - a - 1 | (b, a) <- zip (tail colseps) colseps ]
      colSpecs = zip (map fromCharCol colwidths)
                     (map colAlign
                          (case partSeps of
                             partSep:_ -> partSepColSpec partSep
                             []        -> fromMaybe [] colSpecsFirstLine)
                      ++ repeat AlignDefault)
      tableHead = subtract 1 <$>
                  foldr ((<|>) . (`Map.lookup` rowindex) . partSepLine)
                        Nothing
                        partSeps
  in GridTable
     { gridTableArray = runSTArray (toMutableArray traceInfo rowindex colindex)
     , gridTableHead = tableHead
     , gridTableColSpecs = colSpecs
     }

toMutableArray :: TraceInfo
               -> Map.Map CharRow RowIndex
               -> Map.Map CharCol ColIndex
               -> ST s (STArray s CellIndex (GridCell [Text]))
toMutableArray traceInfo rowindex colindex = do
  let nrows = Map.size rowindex - 1
  let ncols = Map.size colindex - 1
  let gbounds = ( (RowIndex 1, ColIndex 1)
                , (RowIndex nrows, ColIndex ncols)
                )
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
