# Select parts of a JSONC tree-sitter tree

This function is the heart of ts. To edit a tree-sitter tree, you first
need to select the parts you want to delete or update.

This is the S3 method of the
[`tsitter::ts_tree_select()`](https://r-lib.github.io/tsitter/reference/ts_tree_select.html)
generic, for
[ts_tree_jsonc](https://gaborcsardi.github.io/tsjsonc/reference/ts_tree_new.ts_language_jsonc.md)
objects.

## Usage

``` r
# S3 method for class 'ts_tree_jsonc'
ts_tree_select(tree, ..., refine = FALSE)
```

## Arguments

- tree:

  A `ts_tree` object as returned by
  [`ts_tree_new()`](https://r-lib.github.io/tsitter/reference/ts_tree_new.html).

- ...:

  Reserved for future use.

- refine:

  Logical, whether to refine the current selection or start a new
  selection.

## Value

A `ts_tree` object with the selected parts.

## Details

The selection process is iterative. Selection expressions (selectors)
are applied one by one, and each selector selects nodes from the
currently selected nodes. For each selector, it is applied individually
to each currently selected node, and the results are concatenated.

The selection process starts from the root of the DOM tree, the document
node (see
[`ts_tree_dom()`](https://r-lib.github.io/tsitter/reference/ts_tree_dom.html)),
unless `refine = TRUE` is set, in which case it starts from the current
selection.

See the various types of selection expressions below.

### Selectors

#### All elements: `TRUE`

Selects all child nodes of the current nodes.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
    )
    json |> ts_tree_select(c("b", "c"), TRUE)


    #> # jsonc (1 line, 5 selected elements)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

#### Specific keys: character vector

Selects child nodes with the given names from nodes with named children.
If a node has no named children, it selects nothing from that node.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
    )
    json |> ts_tree_select(c("a", "c"), c("c1"))


    #> # jsonc (1 line, 1 selected element)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

#### By position: integer vector

Selects child nodes by position. Positive indices count from the start,
negative indices count from the end. Zero indices are not allowed.

For JSONC positional indices can be used both for arrays and objects.
For other nodes nothing is selected.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
    )
    json |> ts_tree_select(c("b", "c"), -1)


    #> # jsonc (1 line, 2 selected elements)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

#### Matching keys: regular expression

A character scalar named `regex` can be used to select child nodes whose
names match the given regular expression, from nodes with named
children. If a node has no named children, it selects nothing from that
node.

 

    json <- tsjsonc::ts_parse_jsonc(
     '{ "apple": 1, "almond": 2, "banana": 3, "cherry": 4 }'
    )
    json |> ts_tree_select(regex = "^a")


    #> # jsonc (1 line, 2 selected elements)
    #> > 1 | { "apple": 1, "almond": 2, "banana": 3, "cherry": 4 }

#### Tree sitter query matches

A character scalar named `query` can be used to select nodes matching a
tree-sitter query. See
[`ts_tree_query()`](https://r-lib.github.io/tsitter/reference/ts_tree_query.html)
for details on tree-sitter queries.

Instead of a character scalar this can also be a two-element list, where
the first element is the query string and the second element is a
character vector of capture names to select. In this case only nodes
matching the given capture names will be selected.

See
[`ts_language_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_language_jsonc.md)
for details on the JSONC grammar.

This example selects all numbers in the JSON document.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }'
    )
    json |> ts_tree_select(query = "(number) @number")


    #> # jsonc (1 line, 5 selected elements)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }

#### Explicit node ids

You can use `I(c(...))` to select nodes by their ids directly. This is
for advanced use cases only.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
    )
    ts_tree_dom(json)


    #> document (1)
    #> └─object (2)
    #>   ├─number (10) # a
    #>   ├─array (18) # b
    #>   │ ├─number (20)
    #>   │ ├─number (22)
    #>   │ └─number (24)
    #>   └─object (33) # c
    #>     ├─true (41) # c1
    #>     └─null (49) # c2

 

    json |> ts_tree_select(I(18))


    #> # jsonc (1 line, 1 selected element)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

### Refining selections

If the `refine` argument of
[`ts_tree_select()`](https://r-lib.github.io/tsitter/reference/ts_tree_select.html)
is `TRUE`, then the selection starts from the already selected elements
(all of them simultanously), instead of starting from the document
element.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
    )
    json <- json |> ts_tree_select(c("b", "c"))

 

    json |> ts_tree_select(1:2)


    #> # jsonc (1 line, 2 selected elements)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

 

    json |> ts_tree_select(1:2, refine = TRUE)


    #> # jsonc (1 line, 4 selected elements)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

### The `ts_tree_select<-()` replacement function

The
[`ts_tree_select<-()`](https://r-lib.github.io/tsitter/reference/select-set.html)
replacement function works similarly to the combination of
[`ts_tree_select()`](https://r-lib.github.io/tsitter/reference/ts_tree_select.html)
and
[`ts_tree_update()`](https://r-lib.github.io/tsitter/reference/ts_tree_update.html),
but it might be more readable.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
    )
    json


    #> # jsonc (1 line)
    #> 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

 

    json |> ts_tree_select("b", 1)


    #> # jsonc (1 line, 1 selected element)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

 

    ts_tree_select(json, "b", 1) <- 100
    json


    #> # jsonc (1 line)
    #> 1 | { "a": 1, "b": [100, 20, 30], "c": { "c1": true, "c2": null } }

### The `[[` and `[[<-` operators

The `[[` operator works similarly to the combination of
[`ts_tree_select()`](https://r-lib.github.io/tsitter/reference/ts_tree_select.html)
and
[`ts_tree_unserialize()`](https://r-lib.github.io/tsitter/reference/ts_tree_unserialize.html),
but it might be more readable.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
    )
    json |> ts_tree_select("b", 1)


    #> # jsonc (1 line, 1 selected element)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

 

    json[[list("b", 1)]]


    #> [[1]]
    #> [1] 10
    #>

The `[[<-` operator works similarly to the combination of
[`ts_tree_select()`](https://r-lib.github.io/tsitter/reference/ts_tree_select.html)
and
[`ts_tree_update()`](https://r-lib.github.io/tsitter/reference/ts_tree_update.html),
(and also to the replacement function
[`ts_tree_select<-()`](https://r-lib.github.io/tsitter/reference/select-set.html)),
but it might be more readable.

 

    json <- tsjsonc::ts_parse_jsonc(
      '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
    )
    json


    #> # jsonc (1 line)
    #> 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

 

    json |> ts_tree_select("b", 1)


    #> # jsonc (1 line, 1 selected element)
    #> > 1 | { "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }

 

    json[[list("b", 1)]] <- 100
    json


    #> # jsonc (1 line)
    #> 1 | { "a": 1, "b": [100, 20, 30], "c": { "c1": true, "c2": null } }

### Examples

 

    library(tsitter)
    json <- ts_parse_jsonc(ts_serialize_jsonc(list(
      a = list(a1 = list(1,2,3), a2 = "string"),
      b = list(4, 5, 6),
      c = list(c1 = list("a", "b"))
    )))


    #>

 

    json


    #> # jsonc (21 lines)
    #>  1 | {
    #>  2 |   "a": {
    #>  3 |     "a1": [
    #>  4 |       1,
    #>  5 |       2,
    #>  6 |       3
    #>  7 |     ],
    #>  8 |     "a2": "string"
    #>  9 |   },
    #> 10 |   "b": [
    #> ℹ 11 more lines
    #> ℹ Use `print(n = ...)` to see more lines

Select object by key:

 

    json |> ts_tree_select("a")


    #> # jsonc (21 lines, 1 selected element)
    #>    1  | {
    #> >  2  |   "a": {
    #> >  3  |     "a1": [
    #> >  4  |       1,
    #> >  5  |       2,
    #> >  6  |       3
    #> >  7  |     ],
    #> >  8  |     "a2": "string"
    #> >  9  |   },
    #>   10  |   "b": [
    #>   11  |     4,
    #>   12  |     5,
    #>   ...

Select within select, these are the same:

 

    json |> ts_tree_select("a", "a1")
    json |> ts_tree_select(list("a", "a1"))


    #> # jsonc (21 lines, 1 selected element)
    #>    1  | {
    #>    2  |   "a": {
    #> >  3  |     "a1": [
    #> >  4  |       1,
    #> >  5  |       2,
    #> >  6  |       3
    #> >  7  |     ],
    #>    8  |     "a2": "string"
    #>    9  |   },
    #>   10  |   "b": [
    #>   ...
    #> # jsonc (21 lines, 1 selected element)
    #>    1  | {
    #>    2  |   "a": {
    #> >  3  |     "a1": [
    #> >  4  |       1,
    #> >  5  |       2,
    #> >  6  |       3
    #> >  7  |     ],
    #>    8  |     "a2": "string"
    #>    9  |   },
    #>   10  |   "b": [
    #>   ...

Select elements of an array. All elements:

 

    json |> ts_tree_select("b", TRUE)


    #> # jsonc (21 lines, 3 selected elements)
    #>   ...
    #>    8  |     "a2": "string"
    #>    9  |   },
    #>   10  |   "b": [
    #> > 11  |     4,
    #> > 12  |     5,
    #> > 13  |     6
    #>   14  |   ],
    #>   15  |   "c": {
    #>   16  |     "c1": [
    #>   ...

First two elements:

 

    json |> ts_tree_select("b", 1:2)


    #> # jsonc (21 lines, 2 selected elements)
    #>   ...
    #>    8  |     "a2": "string"
    #>    9  |   },
    #>   10  |   "b": [
    #> > 11  |     4,
    #> > 12  |     5,
    #>   13  |     6
    #>   14  |   ],
    #>   15  |   "c": {
    #>   ...

First and last elements:

 

    json |> ts_tree_select("b", c(1, -1))


    #> # jsonc (21 lines, 2 selected elements)
    #>   ...
    #>    8  |     "a2": "string"
    #>    9  |   },
    #>   10  |   "b": [
    #> > 11  |     4,
    #>   12  |     5,
    #> > 13  |     6
    #>   14  |   ],
    #>   15  |   "c": {
    #>   16  |     "c1": [
    #>   ...

Regular expressions:

 

    json |> ts_tree_select(c("a", "c"), regex = "1$")


    #> # jsonc (21 lines, 2 selected elements)
    #>    1  | {
    #>    2  |   "a": {
    #> >  3  |     "a1": [
    #> >  4  |       1,
    #> >  5  |       2,
    #> >  6  |       3
    #> >  7  |     ],
    #>    8  |     "a2": "string"
    #>    9  |   },
    #>   10  |   "b": [
    #>   ...
    #>   13  |     6
    #>   14  |   ],
    #>   15  |   "c": {
    #> > 16  |     "c1": [
    #> > 17  |       "a",
    #> > 18  |       "b"
    #> > 19  |     ]
    #>   20  |   }
    #>   21  | }
