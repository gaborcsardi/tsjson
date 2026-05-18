# Unserialize selected elements from a ts_tree_jsonc object

Uses
[`tsitter::ts_tree_unserialize()`](https://r-lib.github.io/tsitter/reference/ts_tree_unserialize.html)
on the selected elements.

## Usage

``` r
# S3 method for class 'ts_tree_jsonc'
ts_tree_unserialize(tree)
```

## Arguments

- tree:

  ts_tree_jsonc object.

## Value

List of R objects, each the unserialization of a selected element in
tsjsonc.

## Details

If `json` does not have a selection, then all of it is unserialized. If
`json` has an empty selection, then an empty list is returned.

## See also

[`tsitter::ts_tree_unserialize()`](https://r-lib.github.io/tsitter/reference/ts_tree_unserialize.html)
to unserialize a JSON document from a file or string.
[`ts_serialize_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_serialize_jsonc.md)
to create JSON from R objects.

## Examples

``` r
library(tsitter)
json <- ts_parse_jsonc(ts_serialize_jsonc(list(
  a = list(a1 = list(1,2,3), a2 = "string"),
  b = list(4, 5, 6),
  c = list(c1 = list("a", "b"))
)))
json |> ts_tree_select(c("b", "c")) |> ts_tree_unserialize()
#> [[1]]
#> [[1]][[1]]
#> [1] 4
#> 
#> [[1]][[2]]
#> [1] 5
#> 
#> [[1]][[3]]
#> [1] 6
#> 
#> 
#> [[2]]
#> [[2]]$c1
#> [[2]]$c1[[1]]
#> [1] "a"
#> 
#> [[2]]$c1[[2]]
#> [1] "b"
#> 
#> 
#> 
```
