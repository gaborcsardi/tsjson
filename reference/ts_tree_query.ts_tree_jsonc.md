# Run tree-sitter queries on JSONC tree-sitter trees

Use [tree-sitter's query
language](https://tree-sitter.github.io/tree-sitter/) to find nodes in a
tree-sitter tree.

## Usage

``` r
# S3 method for class 'ts_tree_jsonc'
ts_tree_query(tree, query)
```

## Arguments

- tree:

  A `ts_tree` object.

- query:

  Character string, the tree-sitter query to run.

## Value

A list with entries `patterns` and `matched_captures`.

`patterns` contains information about all patterns in the queries and it
is a data frame with columns: `id`, `name`, `pattern`, `match_count`.

`matched_captures` contains information about all matches, and it has
columns `id`, `pattern`, `match`, `start_byte`, `end_byte`, `start_row`,
`start_column`, `end_row`, `end_column`, `name`, `code`.

The `pattern` column of `matched_captured` refers to the `id` column of
`patterns`.

## Details

You probably need to know some details about the specific tree-sitter
parser you are using, to write effective queries. See the documentation
of the parser package you are using for details about the node types and
the query language support. See links below.

## Examples

``` r
# Select all numbers in a JSONC document ------------------------------------
library(tsitter)
json <- tsjsonc::ts_parse_jsonc(
  '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }'
)
json |> ts_tree_query("(number) @number")
#> $patterns
#> # A data frame: 1 × 4
#>      id name  pattern              match_count
#>   <int> <chr> <chr>                      <int>
#> 1     1 NA    "(number) @number\n"           5
#> 
#> $captures
#> # A data frame: 1 × 2
#>      id name  
#>   <int> <chr> 
#> 1     1 number
#> 
#> $matched_captures
#> # A data frame: 5 × 12
#>      id pattern match type   start_byte end_byte start_row start_column
#>   <int>   <int> <int> <chr>       <int>    <int>     <int>        <int>
#> 1     1       1     1 number          7        8         0            7
#> 2     1       1     2 number         16       18         0           16
#> 3     1       1     3 number         20       22         0           20
#> 4     1       1     4 number         24       26         0           24
#> 5     1       1     5 number         54       57         0           54
#> # ℹ 4 more variables: end_row <int>, end_column <int>, name <chr>,
#> #   code <chr>
#> 
```
