{-# LANGUAGE FlexibleContexts  #-}
{-|
Module      : Main
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the gridtables library.
-}
module Main (main) where

import Data.Array (Array, listArray)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Text.GridTable
import Text.GridTable.Parse (tableLine)
import Text.Parsec
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Data.Text as T

main :: IO ()
main = defaultMain $ testGroup "gridtables"
    [ linesTests
    , gridTableTests
    ]

parse' :: ParsecT Text () Identity a -> Text -> Either ParseError a
parse' p t = runParser p () "<test input>" t

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
gridTableTests = testGroup "parseArrayTable"
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
       Right (ArrayTable
              { arrayTableCells = listArray gbounds [ContentCell 1 1 [" one "]]
              , arrayTableHead = Nothing
              , arrayTableColSpecs = defaultAlign [5]
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
       Right (ArrayTable
              { arrayTableCells = listArray gbounds
                                 [ ContentCell 1 1 [" one "]
                                 , ContentCell 1 1 [" two "]
                                 ]
              , arrayTableHead = Nothing
              , arrayTableColSpecs = defaultAlign [5, 5]
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
       Right (ArrayTable
              { arrayTableCells = listArray gbounds
                                 [ ContentCell 1 1 [" 魚 "]
                                 , ContentCell 1 1 [" fish "]
                                 ]
              , arrayTableHead = Nothing
              , arrayTableColSpecs = defaultAlign [4, 6]
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
       Right (ArrayTable
              { arrayTableCells = listArray gbounds
                                 [ ContentCell 1 1 [" one "]
                                 , ContentCell 1 1 [" two "]
                                 ]
              , arrayTableHead = Nothing
              , arrayTableColSpecs = defaultAlign [5]
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
       Right (ArrayTable
              { arrayTableCells = listArray gbounds
                [ ContentCell 2 1 [" one ", "     ", "     "]
                , ContentCell 1 1 [" two   "]
                , ContinuationCell (1, 1)
                , ContentCell 1 1 [" three "]
                ]
              , arrayTableHead = Nothing
              , arrayTableColSpecs = defaultAlign [5, 7]
              })

  , testGroup "table head"
    [ testCase "simple head" $
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
         Right (ArrayTable
                { arrayTableCells = listArray gbounds
                  [ ContentCell 1 1 [" one "]
                  , ContentCell 1 1 [" two "]
                  , ContentCell 1 1 ["  1  "]
                  , ContentCell 1 1 ["  2  "]
                  ]
                , arrayTableHead = Just 1
                , arrayTableColSpecs = defaultAlign [5, 5]
                })

    , testCase "alignment markers" $
      let gt = T.unlines
               [ "+------+--------+-------+"
               , "| left | center | right |"
               , "+:=====+:======:+======:+"
               , "| 1    | 2      | 3     |"
               , "+------+--------+-------+"
               ]
      in parse' gridTable gt @?=
         Right (ArrayTable
                { arrayTableCells = listArray ((1,1), (2, 3))
                  [ ContentCell 1 1 [" left "]
                  , ContentCell 1 1 [" center "]
                  , ContentCell 1 1 [" right "]
                  , ContentCell 1 1 [" 1    "]
                  , ContentCell 1 1 [" 2      "]
                  , ContentCell 1 1 [" 3     "]
                  ]
                , arrayTableHead = Just 1
                , arrayTableColSpecs = listArray (1, 3)
                                       [ (6, AlignLeft)
                                       , (8, AlignCenter)
                                       , (7, AlignRight)
                                       ]
                })
    ]

  , testCase "marker in first line" $
    let gt = T.unlines
             [ "+:-----+:------:+------:+"
             , "| left | center | right |"
             , "+------+--------+-------+"
             , "| a 1  | b 2    | c 3   |"
             , "+------+--------+-------+"
             ]
    in parse' gridTable gt @?=
       Right (ArrayTable
              { arrayTableCells = listArray ((1,1), (2, 3))
                [ ContentCell 1 1 [" left "]
                , ContentCell 1 1 [" center "]
                , ContentCell 1 1 [" right "]
                , ContentCell 1 1 [" a 1  "]
                , ContentCell 1 1 [" b 2    "]
                , ContentCell 1 1 [" c 3   "]
                ]
              , arrayTableHead = Nothing
              , arrayTableColSpecs = listArray (1, 3)
                                     [ (6, AlignLeft)
                                     , (8, AlignCenter)
                                     , (7, AlignRight)
                                     ]
              })

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
       Right (ArrayTable
              { arrayTableCells = listArray gbounds
                                 [ ContentCell 1 1 [" one"]]
              , arrayTableHead = Nothing
              , arrayTableColSpecs = defaultAlign [5]
              })

  , testCase "trailing spaces" $
    let ls = T.unlines
             [ "+---+  "
             , "| 1 | "
             , "+---+"
             ]
    in parse' gridTable ls @?=
       Right (ArrayTable
              { arrayTableCells = listArray ((1,1), (1,1))
                                 [ ContentCell 1 1 [" 1 "]]
              , arrayTableHead = Nothing
              , arrayTableColSpecs = defaultAlign [3]
              })

  , testCase "followed by non-empty line" $
    let ls = T.unlines
             [ "+-----+"
             , "| one |"
             , "+-----+"
             , "text"
             ]
    in parse' (gridTable *> many1 letter) ls @?=
       Right "text"

  , testCase "followed by non-empty line after blank line" $
    let gt = T.unlines
             [ "+-----+"
             , "| one |"
             , "+-----+"
             , ""
             , "Hi Mom!"
             ]
    in parse' (gridTable *> newline *> many1 (letter <|> space)) gt @?=
       Right "Hi Mom"

  , testGroup "access functions"
    [ testCase "rows" $
      let gt = ArrayTable
               { arrayTableCells = listArray ((1, 1), (2, 2))
                 [ ContentCell 2 1 "1"
                 , ContentCell 1 1 "2"
                 , ContinuationCell (1, 1)
                 , ContentCell 1 1 "3"
                 ]
               , arrayTableHead = Nothing
               , arrayTableColSpecs = defaultAlign [5, 7]
               } :: ArrayTable Text
      in rows gt @?= [ [Cell "1" 2 1, Cell "2" 1 1]
                     , [Cell "3" 1 1]
                     ]
    ]
  ]

defaultAlign :: [Int] -> Array ColIndex (Int, Alignment)
defaultAlign widths = listArray (1, ColIndex (length widths))
                    $ map (\w -> (w, AlignDefault)) widths
