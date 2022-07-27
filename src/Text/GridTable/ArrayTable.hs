{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{- |
Module      : Text.GridTable.ArrayTable
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Grid table representation based on arrays.
-}

module Text.GridTable.ArrayTable
  ( GridTable (..)
  , GridCell (..)
  , RowSpan (..)
  , ColSpan (..)
  , CellIndex
  , RowIndex (..)
  , ColIndex (..)
  , Alignment (..)
  , mapCells
  ) where

import Data.Array (Array, Ix)
import Data.Array.MArray (mapArray, thaw)
import Data.Array.ST (runSTArray)

-- | Grid table: Cells are placed on a grid.
data GridTable a = GridTable
  { gridTableArray :: Array CellIndex (GridCell a)
  , gridTableHead  :: Maybe RowIndex
  , gridTableColSpecs :: [(Int, Alignment)]
  }
  deriving stock (Eq, Show)

-- | Apply a function to all cell contents in a grid table.
mapCells :: (a -> b) -> GridTable a -> GridTable b
mapCells f gt =
  let f' = \case
        ContentCell rs cs c  -> ContentCell rs cs $ f c
        ContinuationCell idx -> ContinuationCell idx
      cellArray = runSTArray $ do
        mut <- thaw $ gridTableArray gt
        mapArray f' mut
  in gt { gridTableArray = cellArray }

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
data GridCell a
  = ContentCell RowSpan ColSpan a
  | ContinuationCell CellIndex
  deriving stock (Eq, Show)

-- | The number of rows spanned by a cell.
newtype RowSpan = RowSpan Int
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Read, Show)

-- | The number of columns spanned by a cell.
newtype ColSpan = ColSpan Int
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Read, Show)

-- | Cell alignment
data Alignment
  = AlignDefault
  | AlignLeft
  | AlignCenter
  | AlignRight
  deriving stock (Enum, Eq, Ord, Read, Show)
