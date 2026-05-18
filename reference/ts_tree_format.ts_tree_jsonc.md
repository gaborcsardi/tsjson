# Format the selected JSON elements

Format the selected JSON elements

## Usage

``` r
# S3 method for class 'ts_tree_jsonc'
ts_tree_format(tree, options = NULL, ...)
```

## Arguments

- tree:

  ts_tree_jsonc object.

- options:

  Named list of formatting options, see [tsjsonc
  options](https://gaborcsardi.github.io/tsjsonc/reference/tsjsonc_options.md).

- ...:

  Reserved for future use.

## Value

The updated ts_tree_jsonc object.

## Details

If `tree` does not have a selection, then all of it is formatted. If
`tree` has an empty selection, then nothing happens.

## Examples

``` r
library(tsitter)
tree <- ts_parse_jsonc("{ \"a\": [1,2,3] }")
tree
#> # jsonc (1 line)
#> 1 | { "a": [1,2,3] }

tree |> ts_tree_format()
#> # jsonc (7 lines)
#> 1 | {
#> 2 |     "a": [
#> 3 |         1,
#> 4 |         2,
#> 5 |         3
#> 6 |     ]
#> 7 | }

tree |> ts_tree_select("a") |> ts_tree_format()
#> # jsonc (5 lines)
#> 1 | { "a": [
#> 2 |     1,
#> 3 |     2,
#> 4 |     3
#> 5 | ] }
```
