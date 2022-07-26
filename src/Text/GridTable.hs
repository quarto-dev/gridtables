{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{- |
Module      : Text.GridTable
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Parse reStructuredText-style grid tables.
-}

module Text.GridTable
  ( module Text.GridTable.ArrayTable
  , Cell (..)
  , gridTable
  , tableLine
  , rows
  ) where

import Prelude hiding (lines)
import Control.Applicative ((<|>))
import Data.Array (Array, elems, bounds)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Text.GridTable.ArrayTable
import Text.GridTable.Trace (traceLines)
import Text.Parsec hiding ((<|>))
import qualified Data.Text as T

colBounds :: Array CellIndex a -> (ColIndex, ColIndex)
colBounds = bimap snd snd . bounds

rows :: GridTable a -> [[Cell a]]
rows gt =
  let tarr = gridTableArray gt
      ncols = fromColIndex . uncurry (flip (-)) $ colBounds tarr
      toSimpleCell = \case
        ContentCell rs cs c -> Just $ Cell c rs cs
        ContinuationCell {} -> Nothing
      mkRows :: [[Cell a]] -> [GridCell a] -> [[Cell a]]
      mkRows rs = \case
        [] -> reverse rs
        xs -> let (r, xs') = splitAt (ncols + 1) xs
              in mkRows (mapMaybe toSimpleCell r:rs) xs'
  in mkRows [] $ elems tarr

-- | Raw grid table cell
data Cell a = Cell
  { cellContent :: a
  , cellRowSpan :: RowSpan
  , cellColSpan :: ColSpan
  }
  deriving stock (Eq, Ord, Show)

-- | Parses a grid table.
gridTable :: Stream s m Char => ParsecT s u m (GridTable [Text])
gridTable = try $ do
  firstLine <- (:) <$> char '+' <*> many1 (oneOf "+-") <* skipSpaces <* newline
  lines <- many1 tableLine
  case traceLines (T.pack firstLine : lines) of
    Nothing -> fail "tracing failed"
    Just gt -> return gt

skipSpaces :: Stream s m Char => ParsecT s u m ()
skipSpaces = skipMany (satisfy $ \c -> c == '\t' || c == ' ')

-- | Parses a line that's part of a table. The line must start with
-- either a plus @+@ or a pipe @|@.
tableLine :: Stream s m Char
          => ParsecT s u m Text
tableLine = try $ do
  let borderChar = char '+' <|> char '|'
  firstChar <- borderChar
  rest <- manyTill (noneOf "\n\r") newline
  return $ T.stripEnd $ T.pack (firstChar : rest)

