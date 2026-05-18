# tsjsonc

Extract and manipulate parts of JSON files without touching the
formatting and comments in other parts.

## Installation

You can install the development version of tsjsonc from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("gaborcsardi/tsjsonc")
```

## Documentation

See at
[`https://gaborcsardi.github.io/tsjsonc/`](https://gaborcsardi.github.io/tsjsonc/reference/index.html)
and also in the installed package:
[`help(package = "tsjsonc")`](https://gaborcsardi.github.io/tsjsonc/reference).

## Quickstart

### Create a ts_tree_jsonc object

Create a ts_tree_jsonc object from a string:

``` r

txt <- r"(
// this is a comment
{
  "a": {
    "a1": [1, 2, 3],
    // comment
    "a2": "string"
  },
  "b": [
    {
      "b11": true,
      "b12": false
    },
    {
      "b21": false,
      "b22": false
    }
  ]
}
)"
json <- ts_parse_jsonc(text = txt)
```

Pretty print a ts_tree_jsonc object:

``` r

json
```


    #> # jsonc (19 lines)
    #>  1 | 
    #>  2 | // this is a comment
    #>  3 | {
    #>  4 |   "a": {
    #>  5 |     "a1": [1, 2, 3],
    #>  6 |     // comment
    #>  7 |     "a2": "string"
    #>  8 |   },
    #>  9 |   "b": [
    #> 10 |     {
    #> ℹ 9 more lines
    #> ℹ Use `print(n = ...)` to see more lines

### Select elements in a ts_tree_jsonc object

Select element by objects key:

``` r

ts_tree_select(json, "a")
```


    #> # jsonc (19 lines, 1 selected element)
    #>    1  | 
    #>    2  | // this is a comment
    #>    3  | {
    #> >  4  |   "a": {
    #> >  5  |     "a1": [1, 2, 3],
    #> >  6  |     // comment
    #> >  7  |     "a2": "string"
    #> >  8  |   },
    #>    9  |   "b": [
    #>   10  |     {
    #>   11  |       "b11": true,
    #>   ...

Select element inside element:

``` r

ts_tree_select(json, "a", "a1")
```


    #> # jsonc (19 lines, 1 selected element)
    #>   2   | // this is a comment
    #>   3   | {
    #>   4   |   "a": {
    #> > 5   |     "a1": [1, 2, 3],
    #>   6   |     // comment
    #>   7   |     "a2": "string"
    #>   8   |   },
    #>   ...

Select element(s) of an array:

``` r

ts_tree_select(json, "a", "a1", 1:2)
```


    #> # jsonc (19 lines, 2 selected elements)
    #>   2   | // this is a comment
    #>   3   | {
    #>   4   |   "a": {
    #> > 5   |     "a1": [1, 2, 3],
    #>   6   |     // comment
    #>   7   |     "a2": "string"
    #>   8   |   },
    #>   ...

Select multiple keys from an object:

``` r

ts_tree_select(json, "a", c("a1", "a2"))
```


    #> # jsonc (19 lines, 2 selected elements)
    #>    2  | // this is a comment
    #>    3  | {
    #>    4  |   "a": {
    #> >  5  |     "a1": [1, 2, 3],
    #>    6  |     // comment
    #> >  7  |     "a2": "string"
    #>    8  |   },
    #>    9  |   "b": [
    #>   10  |     {
    #>   ...

Select nodes that match a tree-sitter query:

``` r

json |> ts_tree_select(query = "((pair value: (false) @val))")
```


    #> # jsonc (19 lines, 3 selected elements)
    #>   ...
    #>    9  |   "b": [
    #>   10  |     {
    #>   11  |       "b11": true,
    #> > 12  |       "b12": false
    #>   13  |     },
    #>   14  |     {
    #> > 15  |       "b21": false,
    #> > 16  |       "b22": false
    #>   17  |     }
    #>   18  |   ]
    #>   19  | }

### Delete elements

Delete selected elements:

``` r

ts_tree_select(json, "a", "a1") |> ts_tree_delete()
```


    #> # jsonc (18 lines)
    #>  1 | 
    #>  2 | // this is a comment
    #>  3 | {
    #>  4 |   "a": {
    #>  5 |     // comment
    #>  6 |     "a2": "string"
    #>  7 |   },
    #>  8 |   "b": [
    #>  9 |     {
    #> 10 |       "b11": true,
    #> ℹ 8 more lines
    #> ℹ Use `print(n = ...)` to see more lines

### Insert elements

Insert element into an array:

``` r

ts_tree_select(json, "a", "a1") |> ts_tree_insert(at = 2, "new")
```


    #> # jsonc (24 lines)
    #>  1 | 
    #>  2 | // this is a comment
    #>  3 | {
    #>  4 |   "a": {
    #>  5 |     "a1": [
    #>  6 |         1,
    #>  7 |         2,
    #>  8 |         "new",
    #>  9 |         3
    #> 10 |     ],
    #> ℹ 14 more lines
    #> ℹ Use `print(n = ...)` to see more lines

Inserting into an array reformats the array.

Insert element into an object, at the specified key:

``` r

ts_tree_select(json, "a") |>
  ts_tree_insert(key = "a0", at = 0, list("new", "element"))
```


    #> # jsonc (27 lines)
    #>  1 | 
    #>  2 | // this is a comment
    #>  3 | {
    #>  4 |   "a": {
    #>  5 |       "a0": [
    #>  6 |           "new",
    #>  7 |           "element"
    #>  8 |       ],
    #>  9 |       "a1": [
    #> 10 |           1,
    #> ℹ 17 more lines
    #> ℹ Use `print(n = ...)` to see more lines

### Update elements

Update existing element:

``` r

ts_tree_select(json, "a", c("a1", "a2")) |> ts_tree_update("new value")
```


    #> # jsonc (19 lines)
    #>  1 | 
    #>  2 | // this is a comment
    #>  3 | {
    #>  4 |   "a": {
    #>  5 |     "a1": "new value",
    #>  6 |     // comment
    #>  7 |     "a2": "new value"
    #>  8 |   },
    #>  9 |   "b": [
    #> 10 |     {
    #> ℹ 9 more lines
    #> ℹ Use `print(n = ...)` to see more lines

Inserts the element if some parents are missing:

``` r

json <- ts_parse_jsonc(text = "{ \"a\": { \"b\": true } }")
json
```


    #> # jsonc (1 line)
    #> 1 | { "a": { "b": true } }

``` r

ts_tree_select(json, "a", "x", "y") |> ts_tree_update(list(1,2,3))
```


    #> # jsonc (10 lines)
    #>  1 | { "a": {
    #>  2 |     "b": true,
    #>  3 |     "x": {
    #>  4 |         "y": [
    #>  5 |             1,
    #>  6 |             2,
    #>  7 |             3
    #>  8 |         ]
    #>  9 |     }
    #> 10 | } }

# License

MIT © Posit Software, PBC
