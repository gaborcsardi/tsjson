# Insert a new element into the selected ones in a ts_tree_jsonc object

Insert a new element into each selected array or object.

## Usage

``` r
# S3 method for class 'ts_tree_jsonc'
ts_tree_insert(tree, new, key = NULL, at = Inf, options = NULL, ...)
```

## Arguments

- tree:

  ts_tree_jsonc object

- new:

  New element to insert. Will be serialized with
  [`ts_serialize_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_serialize_jsonc.md).

- key:

  Key of the new element, when inserting into an object.

- at:

  What position to insert the new element at:

  - `0`: at the beginning,

  - `Inf`: at the end,

  - other numbers: after the specified element,

  - a character scalar, the key after which the new element is inserted,
    if that key exists, when inserting into an object. If this key does
    not exist, then the new element is inserted at the end of the
    object.

- options:

  Named list of formatting options, see [tsjsonc
  options](https://gaborcsardi.github.io/tsjsonc/reference/tsjsonc_options.md).

- ...:

  Must be empty currently, reserved for future use.

## Value

The modified ts_tree_jsonc object.

## Details

It is an error trying to insert into an element that is not an array and
not an object.

## Examples

``` r
library(tsitter)
json <- ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
json
#> # jsonc (1 line)
#> 1 | { "a": true, "b": [1, 2, 3] }

json |> ts_tree_select("b") |> ts_tree_insert("foo", at = 1)
#> # jsonc (6 lines)
#> 1 | { "a": true, "b": [
#> 2 |     1,
#> 3 |     "foo",
#> 4 |     2,
#> 5 |     3
#> 6 | ] }
```
