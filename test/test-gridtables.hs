{-|
Module      : Main
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the gridtables library.
-}
module Main (main) where

import Data.Text (Text)
import Text.GridTable
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

main :: IO ()
main = defaultMain $ testGroup "gridtables"
    [ placeholderTest
    ]

-- | Basic tests
placeholderTest :: TestTree
placeholderTest = testGroup "placeholder"
  [ testCase "check value" $
    placeholder @?= ("placeholder" :: Text)
  ]
