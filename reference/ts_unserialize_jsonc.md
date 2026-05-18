# Unserialize a JSON file or string into an R object

The purpose of this function is to convert a JSON file or string into an
R object reliably.

## Usage

``` r
ts_unserialize_jsonc(file = NULL, text = NULL, ranges = NULL, options = NULL)
```

## Arguments

- file:

  Path of a file to parse. Use either `file` or `text`, but not both.

- text:

  String to parse. Use either `file` or `text`, but not both.

- ranges:

  Can be used to parse part(s) of the input. It must be a data frame
  with integer columns `start_row`, `start_col`, `end_row`, `end_col`,
  `start_byte`, `end_byte`, in this order.

- options:

  Named list of parsing options, see [tsjsonc
  options](https://gaborcsardi.github.io/tsjsonc/reference/tsjsonc_options.md).

## Value

R object.

## Details

See examples below on how the different JSON elements are mapped to R
objects.

## See also

[`ts_serialize_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_serialize_jsonc.md)
for the opposite,
[`tsitter::ts_tree_select()`](https://r-lib.github.io/tsitter/reference/ts_tree_select.html)
and
[`tsitter::ts_tree_unserialize()`](https://r-lib.github.io/tsitter/reference/ts_tree_unserialize.html)
to unserialize part(s) of a JSON document.
[`tsitter::ts_tree_new()`](https://r-lib.github.io/tsitter/reference/ts_tree_new.html)
to load a JSON document and then manipulate it.

## Examples

``` r
library(tsitter)
# null -> NULL
ts_unserialize_jsonc(text = "null")
#> NULL

# true, false -> TRUE, FALSE
ts_unserialize_jsonc(text = "true")
#> [1] TRUE
ts_unserialize_jsonc(text = "false")
#> [1] FALSE

# string -> character scalar
ts_unserialize_jsonc(text = "\"string with escapes: \\b \\ud020\"")
#> [1] "string with escapes: \b 퀠"

# number -> double scalar
ts_unserialize_jsonc(text = "42.25")
#> [1] 42.25

# array -> unnamed list
ts_unserialize_jsonc(text = "[1, 2, 3]")
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 

# object -> named list
ts_unserialize_jsonc(text = "{\"a\": 1, \"b\": 2 }")
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
```
