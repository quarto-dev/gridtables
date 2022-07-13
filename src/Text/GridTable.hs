{-# LANGUAGE FlexibleContexts  #-}
{- |
Copyright:  © 2022 Albert Krewinkel
License:    MIT
Maintainer: Albert Krewinkel <albert@zeitkraut.de>

Parse reStructuredText-style grid tables.
-}

module Text.GridTable
  ( GridTable (..)
  , Lines (..)
  , gridTable
  , tableLines
  ) where

import Prelude hiding (lines)
import Control.Monad (void)
import Data.Text (Text)
import Text.Parsec
import qualified Data.Text as T

-- | Raw grid table.
data GridTable = GridTable
  deriving (Eq, Show)

-- | Parses a grid table.
gridTable :: Stream s m Char
          => ParsecT s u m GridTable
gridTable = do
  _ <- tableLines
  return GridTable

-- | All lines of text that make up a table.
--
-- Instances for 'Show' and 'Eq' are derived for testing purposes.
newtype Lines = Lines [Text]
  deriving (Eq, Show)

-- | Parses the lines of a grid table.
tableLines :: Stream s m Char
           => ParsecT s u m Lines
tableLines = Lines <$>
  tableLine `manyTill` (eof <|> lookAhead (void newline))

-- | Parses a line that's part of a table. The line must start with
-- either a plus @+@ or a pipe @|@.
tableLine :: Stream s m Char
          => ParsecT s u m Text
tableLine = try $ do
  firstChar <- char '+' <|> char '|'
  rest <- manyTill anyChar (char firstChar *> newline)
  return $ firstChar `T.cons` T.pack rest `T.snoc` firstChar
