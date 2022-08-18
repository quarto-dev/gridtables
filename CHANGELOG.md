# Changelog

`gridtables` uses [PVP Versioning][].

## gridtables-0.0.3.0

Released 2022-08-18.

-   Missing cells no longer cause an error, but are replaced with
    empty cells.

-   The borders of the last cell in a row are allowed to be
    shorter than the cell. Previously the last column was
    discarded in that case.

## gridtables-0.0.2.0

Released 2022-07-30.

-   Treat "combining" Unicode characters, such as the zero-width
    space or the word joiner, as having no width.


## gridtables-0.0.1.0

Released 2022-07-29.

-   Boldly going where no Haskell library has gone before.

  [PVP Versioning]: https://pvp.haskell.org
