# gridtables

Parser for reStructuredText-style grid tables.

This package provides a parser for plain-text representations of
tables, like the one given below.

```
+---------------------+-----------------------+
| Location            | Temperature 1961-1990 |
|                     | in degree Celsius     |
|                     +-------+-------+-------+
|                     | min   | mean  | max   |
+=====================+=======+=======+=======+
| Antarctica          | -89.2 | N/A   | 19.8  |
+---------------------+-------+-------+-------+
| Earth               | -89.2 | 14    | 56.7  |
+---------------------+-------+-------+-------+
```

## Character widths

The tables are intended to look good when viewed in a monospace
font. Therefore, wide and full-width characters, as those in East
Asian scripts, are counted as two characters, while zero-width and
combining characters are treated as if they have no width.

## Column alignments

The parser re-implements a table extensions from John MacFarlane's
pandoc, namely support for column-wide cell alignments. The
alignment of cells is determined by placing colons in the row that
separates the table head from the body, like so:

    +------+--------+-------+
    | left | center | right |
    +:=====+:======:+======:+
    | 1    | 2      | 3     |
    +------+--------+-------+

The first line must be used for headless tables:

    +:-----+:------:+------:+
    | left | center | right |
    +------+--------+-------+
    | a 1  | b 2    | c 3   |
    +------+--------+-------+


## Algorithm

The cell tracing algorithm used in this package has been
translated from the original Python implementation for
reStructuredText. The parser has been placed in the public domain.

## Usage

The usual way to use this package will be to use it as part of a
parsec parser:

``` haskell
main :: IO ()
main = do
  let gt = T.unlines
           [ "+------+--------+-------+"
           , "| left | center | right |"
           , "+:=====+:======:+======:+"
           , "| 1    | 2      | 3     |"
           , "+------+--------+-------+"
           ]
  in print (runParser GT.gridTable () "table" gt)
```

Use `traceLines :: [Text] -> Maybe (GridTable [Text])`, if the
table's raw lines have been retrieved in a different way.
