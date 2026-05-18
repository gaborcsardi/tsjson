# Update selected elements in a ts_tree_jsonc object

Update the selected elements of a JSON document, using the replacement
function syntax.

## Arguments

- x, json:

  ts_tree_jsonc object. Create a ts_tree_jsonc object with
  [`tsitter::ts_tree_new()`](https://r-lib.github.io/tsitter/reference/ts_tree_new.html).

- i, ...:

  Selectors, see
  [`tsitter::ts_tree_select()`](https://r-lib.github.io/tsitter/reference/ts_tree_select.html).

- value:

  New value. Will be serialized to JSON with
  [`ts_serialize_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_serialize_jsonc.md).

## Value

The updated ts_tree_jsonc object.

## See also

Save the updated ts_tree_jsonc object to a file with
[`tsitter::ts_tree_write()`](https://r-lib.github.io/tsitter/reference/ts_tree_write.html).

## Examples

``` r
library(tsitter)
json <- ts_parse_jsonc("{}")

json <- json |>
  ts_tree_select("r", "editor.formatOnSave") |>
  ts_tree_update(TRUE)
json
#> # jsonc (5 lines)
#> 1 | {
#> 2 |     "r": {
#> 3 |         "editor.formatOnSave": true
#> 4 |     }
#> 5 | }

json <- json |>
  ts_tree_select("r", "editor.formatOnSave") |>
  ts_tree_delete()
json
#> # jsonc (4 lines)
#> 1 | {
#> 2 |     "r": {
#> 3 |         }
#> 4 | }

# Insert an array
json <- json |>
  ts_tree_select("foo") |>
  ts_tree_update(1:3)
json
#> # jsonc (8 lines)
#> 1 | {
#> 2 |     "r": {},
#> 3 |     "foo": [
#> 4 |         1,
#> 5 |         2,
#> 6 |         3
#> 7 |     ]
#> 8 | }

# Update the array at location 2
json |> ts_tree_select("foo", 2) |> ts_tree_update(0)
#> # jsonc (8 lines)
#> 1 | {
#> 2 |     "r": {},
#> 3 |     "foo": [
#> 4 |         1,
#> 5 |         0,
#> 6 |         3
#> 7 |     ]
#> 8 | }

# Insert at location 2
json |> ts_tree_select("foo") |> ts_tree_insert(0, at = 2)
#> # jsonc (9 lines)
#> 1 | {
#> 2 |     "r": {},
#> 3 |     "foo": [
#> 4 |         1,
#> 5 |         2,
#> 6 |         0,
#> 7 |         3
#> 8 |     ]
#> 9 | }

# Insert at the end of the array with `Inf` as `at`
json |> ts_tree_select("foo") |> ts_tree_insert(0, at = Inf)
#> # jsonc (9 lines)
#> 1 | {
#> 2 |     "r": {},
#> 3 |     "foo": [
#> 4 |         1,
#> 5 |         2,
#> 6 |         3,
#> 7 |         0
#> 8 |     ]
#> 9 | }

# Only the modified elements are reformatted
json <- ts_parse_jsonc('{"foo":[1,2],\n"bar":1}')
json |> ts_tree_select("foo") |> ts_tree_insert(0, at = Inf)
#> # jsonc (6 lines)
#> 1 | {"foo":[
#> 2 |     1,
#> 3 |     2,
#> 4 |     0
#> 5 | ],
#> 6 | "bar":1}

# You can control how those elements are formatted
json |> ts_tree_select("foo") |>
  ts_tree_insert(0, at = Inf, options = list(indent_width = 2))
#> # jsonc (6 lines)
#> 1 | {"foo":[
#> 2 |   1,
#> 3 |   2,
#> 4 |   0
#> 5 | ],
#> 6 | "bar":1}
```
