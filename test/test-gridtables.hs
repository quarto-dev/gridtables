{-# LANGUAGE FlexibleContexts  #-}
{-|
Module      : Main
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the gridtables library.
-}
module Main (main) where

import Data.Functor.Identity (Identity)
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

parse' :: GridTableParser Identity a -> Text -> Either ParseError a
parse' p t = runParser p emptyGridInfo "<test input>" t

-- | Test parsing into lines
linesTests :: TestTree
linesTests = testGroup "lines"
  [ testCase "get lines" $
    parse' tableLines "| one | two |\n| three |\n| four |\n"
    @?= Right ([ Line 1 "| one | two |"
               , Line 2 "| three |"
               , Line 3 "| four |" ])

  , testCase "fail if not a table" $
    parse' tableLines "nope\nnada\n" @?= Right []
  ]

-- | Test parsing of a text as grid tables.
gridTableTests :: TestTree
gridTableTests = testGroup "parseGridTable"
  [ testCase "single cell" $
    let gt = T.unlines
             [ "+-----+"
             , "| one |"
             , "+-----+"
             ]
    in parse' gridTable gt @?= Right (GridTable [Cell [" one "]])

  , testCase "multi-cell row" $
    let gt = T.unlines
             [ "+-----+-----+"
             , "| one | two |"
             , "+-----+-----+"
             ]
    in parse' gridTable gt @?=
       Right (GridTable [Cell [" one "], Cell [" two "]])


  , testCase "unterminated row" $
    let gt = T.unlines
             [ "+-----+"
             , "| one"
             , "+-----+"
             ]
    in assertBool "" . isLeft $ parse' gridTable gt

  , testCase "followed by non-empty line" $
    let gt = T.unlines
             [ "+-----+"
             , "| one |"
             , "+-----+"
             , "text"
             ]
    in assertBool "" . isLeft $ parse' gridTable gt
  ]
