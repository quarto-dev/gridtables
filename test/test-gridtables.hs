{-# LANGUAGE FlexibleContexts  #-}
{-|
Module      : Main
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the gridtables library.
-}
module Main (main) where

import Data.Array (listArray)
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
    parse' (many1 tableLine) "| one | two |\n| three |\n| four |\n"
    @?= Right ([ "| one | two |"
               , "| three |"
               , "| four |"
               ])

  , testCase "fail if not a table" $
    parse' (many tableLine) "nope\nnada\n" @?= Right []
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
        gbounds = ( (RowIndex 1, ColIndex 1)
                  , (RowIndex 1, ColIndex 1)
                  )
    in parse' gridTable gt @?=
       Right (GridTable
              { gridTableArray = listArray gbounds [ContentCell 1 1 [" one "]]
              , gridTableHead = Nothing
              , gridTableColWidths = [5]
              })

  , testCase "multi-cell row" $
    let gt = T.unlines
             [ "+-----+-----+"
             , "| one | two |"
             , "+-----+-----+"
             ]
        gbounds = ( (RowIndex 1, ColIndex 1)
                  , (RowIndex 1, ColIndex 2)
                  )
    in parse' gridTable gt @?=
       Right (GridTable
              { gridTableArray = listArray gbounds
                                 [ ContentCell 1 1 [" one "]
                                 , ContentCell 1 1 [" two "]
                                 ]
              , gridTableHead = Nothing
              , gridTableColWidths = [5, 5]
              })


  , testCase "wide character" $
    let gt = T.unlines
             [ "+----+------+"
             , "| 魚 | fish |"
             , "+----+------+"
             ]
        gbounds = ( (RowIndex 1, ColIndex 1)
                  , (RowIndex 1, ColIndex 2)
                  )
    in parse' gridTable gt @?=
       Right (GridTable
              { gridTableArray = listArray gbounds
                                 [ ContentCell 1 1 [" 魚 "]
                                 , ContentCell 1 1 [" fish "]
                                 ]
              , gridTableHead = Nothing
              , gridTableColWidths = [4, 6]
              })

  , testCase "two-row table" $
    let gt = T.unlines
             [ "+-----+"
             , "| one |"
             , "+-----+"
             , "| two |"
             , "+-----+"
             ]
        gbounds = ( (RowIndex 1, ColIndex 1)
                  , (RowIndex 2, ColIndex 1)
                  )
    in parse' gridTable gt @?=
       Right (GridTable
              { gridTableArray = listArray gbounds
                                 [ ContentCell 1 1 [" one "]
                                 , ContentCell 1 1 [" two "]
                                 ]
              , gridTableHead = Nothing
              , gridTableColWidths = [5]
              })

  , testCase "rowspan" $
    let gt = T.unlines
             [ "+-----+-------+"
             , "| one | two   |"
             , "|     +-------+"
             , "|     | three |"
             , "+-----+-------+"
             ]
        gbounds = ( (RowIndex 1, ColIndex 1)
                  , (RowIndex 2, ColIndex 2)
                  )
    in parse' gridTable gt @?=
       Right (GridTable
              { gridTableArray = listArray gbounds
                [ ContentCell 2 1 [" one ", "     ", "     "]
                , ContentCell 1 1 [" two   "]
                , ContinuationCell (1, 1)
                , ContentCell 1 1 [" three "]
                ]
              , gridTableHead = Nothing
              , gridTableColWidths = [5, 7]
              })

  , testGroup "table head"
    [ testCase "rowspan" $
      let gt = T.unlines
               [ "+-----+-----+"
               , "| one | two |"
               , "+=====+=====+"
               , "|  1  |  2  |"
               , "+-----+-----+"
               ]
          gbounds = ( (RowIndex 1, ColIndex 1)
                    , (RowIndex 2, ColIndex 2)
                    )
      in parse' gridTable gt @?=
         Right (GridTable
                { gridTableArray = listArray gbounds
                  [ ContentCell 1 1 [" one "]
                  , ContentCell 1 1 [" two "]
                  , ContentCell 1 1 ["  1  "]
                  , ContentCell 1 1 ["  2  "]
                  ]
                , gridTableHead = Just 1
                , gridTableColWidths = [5, 5]
                })
    ]

  , testCase "unterminated row" $
    let gt = T.unlines
             [ "+-----+"
             , "| one"
             , "+-----+"
             ]
        gbounds = ( (RowIndex 1, ColIndex 1)
                  , (RowIndex 1, ColIndex 1)
                  )
    in parse' gridTable gt @?=
       Right (GridTable
              { gridTableArray = listArray gbounds
                                 [ ContentCell 1 1 [" one"]]
              , gridTableHead = Nothing
                , gridTableColWidths = [5]
              })

  , testCase "followed by non-empty line" $
    let gt = T.unlines
             [ "+-----+"
             , "| one |"
             , "+-----+"
             , "text"
             ]
    in assertBool "" . isLeft $ parse' gridTable gt
  ]
