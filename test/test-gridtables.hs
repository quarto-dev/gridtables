{-# LANGUAGE FlexibleContexts  #-}
{-|
Module      : Main
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the gridtables library.
-}
module Main (main) where

import Data.Either (isLeft)
import Data.Text (Text)
import Text.GridTable
import Text.Parsec
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

import qualified Data.Text as T

main :: IO ()
main = defaultMain $ testGroup "gridtables"
    [ linesTests
    , gridTableTests
    ]

parse' :: Parsec Text () a -> Text -> Either ParseError a
parse' p t = parse p "<test input>" t

-- | Test parsing into lines
linesTests :: TestTree
linesTests = testGroup "lines"
  [ testCase "get lines" $
    parse' tableLines "| one two |\n| three |\n| four |\n"
    @?= Right (Lines [ "| one two |", "| three |", "| four |" ])

  , testCase "fail if not a table" $
    assertBool "non-table cannot be parsed" .
      isLeft $ parse' tableLines "nope\nnada\n"
  ]

-- | Test parsing of a text as grid tables.
gridTableTests :: TestTree
gridTableTests = testGroup "parseGridTable"
  [ testCase "success" $
    let gt = T.unlines
             [ "+-----+"
             , "| one |"
             , "+-----+"
             ]
    in parse' gridTable gt @?= Right GridTable
  ]
