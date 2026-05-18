# Replace selected JSON elements with a new element

Replace all selected elements with a new element. If `tree` has no
selection then the whole document is replaced.

## Usage

``` r
# S3 method for class 'ts_tree_jsonc'
ts_tree_update(tree, new, options = NULL, ...)
```

## Arguments

- tree:

  ts_tree_jsonc object.

- new:

  R object that will be serialized to JSON (using
  [`ts_serialize_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_serialize_jsonc.md))
  and inserted in place of the selected JSON elements.

- options:

  Named list of formatting options, see [tsjsonc
  options](https://gaborcsardi.github.io/tsjsonc/reference/tsjsonc_options.md).

- ...:

  Reserved for future use.

## Value

The updated ts_tree_jsonc object

## Examples

``` r
library(tsitter)
tree <- ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
tree
#> # jsonc (1 line)
#> 1 | { "a": true, "b": [1, 2, 3] }

tree |> ts_tree_select("a") |> ts_tree_update(list("new", "element"))
#> # jsonc (4 lines)
#> 1 | { "a": [
#> 2 |   "new",
#> 3 |   "element"
#> 4 | ], "b": [1, 2, 3] }
```
