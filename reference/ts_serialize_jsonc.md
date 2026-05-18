# Serialize an R object to JSON

Create JSON from an R object. Note that this function is not a generic
serializer that can represent any R object in JSON. Also, you cannot
expect that
[`ts_unserialize_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_unserialize_jsonc.md)
will do the exact inverse of `ts_serialize_jsonc()`.

## Usage

``` r
ts_serialize_jsonc(obj, file = NULL, collapse = FALSE, options = NULL)
```

## Arguments

- obj:

  R object to serialize.

- file:

  If not `NULL` then the result if written to this file.

- collapse:

  If `file` is `NULL` then whether to return a character scalar or a
  character vector.

- options:

  A list of options for the formatting.

  See details in the manual of the specific parser.

## Value

If `file` is `NULL` then a character scalar (`collapse` = TRUE) or
vector (`collapse` = FALSE). If `file` is not `NULL` then nothing.

## Details

tsjsonc functions
[`tsitter::ts_tree_update()`](https://r-lib.github.io/tsitter/reference/ts_tree_update.html)
and
[`tsitter::ts_tree_insert()`](https://r-lib.github.io/tsitter/reference/ts_tree_insert.html)
use `ts_serialize_jsonc()` to create new JSON code.

See the examples below on how to create all possible JSON elements with
`ts_serialize_jsonc()`.

## See also

[`ts_unserialize_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_unserialize_jsonc.md)
for the opposite.

## Examples

``` r
library(tsitter)
# null
ts_serialize_jsonc(NULL)
#> [1] "null"

# true, false, use a logical scalar
ts_serialize_jsonc(TRUE)
#> [1] "true"
ts_serialize_jsonc(FALSE)
#> [1] "false"

# strings, use a character scalar
ts_serialize_jsonc("string with escapes: \b \ud020")
#> [1] "\"string with escapes: \\b 퀠\""

# number, use a numeric scalar
ts_serialize_jsonc(42.25)
#> [1] "42.25"

# array, use an unnamed list, i.e. _not_ an atomic vector
txt <- ts_serialize_jsonc(list(1, 2, 3,"x", "y"))
ts_parse_jsonc(txt)
#> # jsonc (7 lines)
#> 1 | [
#> 2 |   1,
#> 3 |   2,
#> 4 |   3,
#> 5 |   "x",
#> 6 |   "y"
#> 7 | ]

# empty array
ts_serialize_jsonc(list())
#> [1] "[]"

# object, use a named (or partially named) list, i.e. _not_ an atomic vector
txt <- ts_serialize_jsonc(list(a = 1, b = 2))
ts_parse_jsonc(txt)
#> # jsonc (4 lines)
#> 1 | {
#> 2 |   "a": 1,
#> 3 |   "b": 2
#> 4 | }

# empty object, use a named empty list
ts_serialize_jsonc(structure(list(), names = character()))
#> [1] "{}"
```
