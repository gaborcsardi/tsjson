# Delete selected elements from a ts_tree_jsonc object

Delete selected elements from a ts_tree_jsonc object

## Usage

``` r
# S3 method for class 'ts_tree_jsonc'
ts_tree_delete(tree, ...)
```

## Arguments

- tree:

  A `ts_tree` object.

- ...:

  Reserved for future use.

## Value

The modified `ts_tree` object with the selected elements removed.

## Details

The formatting of the rest of the document is left as is.

 

    jsonc <- tsjsonc::ts_parse_jsonc(
      "{ \"a\": true, \"b\": [1, 2, 3] }"
    ) |>
      tsitter::ts_tree_format()
    jsonc


    #> # jsonc (8 lines)
    #> 1 | {
    #> 2 |     "a": true,
    #> 3 |     "b": [
    #> 4 |         1,
    #> 5 |         2,
    #> 6 |         3
    #> 7 |     ]
    #> 8 | }

 

    jsonc |> ts_tree_select("a") |> ts_tree_delete()


    #> # jsonc (7 lines)
    #> 1 | {
    #> 2 |     "b": [
    #> 3 |         1,
    #> 4 |         2,
    #> 5 |         3
    #> 6 |     ]
    #> 7 | }

If the tree does not have a selection, the tree corresponding to the
empty document is returned, i.e. the whole content is deleted.

 

    jsonc <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
    jsonc |> ts_tree_delete()


    #> # jsonc (0 lines)

If the tree has a selection, but it is the empty selection, then the
tree is returned unchanged.

 

    jsonc <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
    jsonc |> ts_tree_select("c") |> ts_tree_delete()


    #> # jsonc (1 line)
    #> 1 | { "a": true, "b": [1, 2, 3] }

For parsers that support comments, deleting elements that include
comments typically delete the comments as well. Other comments are kept
as is. See details in the manual of the specific parser.

 

    jsonc <- tsjsonc::ts_parse_jsonc(
      "// top comment\n{ \"a\": // comment\n  true,\n \"b\": [1, 2, 3] }"
    ) |> tsitter::ts_tree_format()
    jsonc


    #> # jsonc (11 lines)
    #>  1 | // top comment
    #>  2 | {
    #>  3 |     "a":
    #>  4 |         // comment
    #>  5 |         true,
    #>  6 |     "b": [
    #>  7 |         1,
    #>  8 |         2,
    #>  9 |         3
    #> 10 |     ]
    #> ℹ 1 more line
    #> ℹ Use `print(n = ...)` to see more lines

 

    jsonc |> ts_tree_select("a") |> ts_tree_delete()


    #> # jsonc (8 lines)
    #> 1 | // top comment
    #> 2 | {
    #> 3 |     "b": [
    #> 4 |         1,
    #> 5 |         2,
    #> 6 |         3
    #> 7 |     ]
    #> 8 | }

Comments appearing inside the deleted elements are also deleted. Other
comments are left as is.

## Examples

``` r
library(tsitter)
tree <- ts_parse_jsonc("{ \"a\": //comment\ntrue, \"b\": [1, 2, 3] }")
tree
#> # jsonc (2 lines)
#> 1 | { "a": //comment
#> 2 | true, "b": [1, 2, 3] }

tree |> ts_tree_select("a")
#> # jsonc (2 lines, 1 selected element)
#> > 1 | { "a": //comment
#>   2 | true, "b": [1, 2, 3] }
tree |> ts_tree_select("a") |> ts_tree_delete()
#> # jsonc (1 line)
#> 1 | { "b": [1, 2, 3] }
```
