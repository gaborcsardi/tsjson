# Parse a JSON file or string into a ts_tree_jsonc object

Parse a JSON file or string and create a ts_tree_jsonc object that
represents its document. This object can then be queried and
manipulated.

## Usage

``` r
ts_parse_jsonc(text, ranges = NULL, fail_on_parse_error = TRUE, options = NULL)

ts_read_jsonc(file, ranges = NULL, fail_on_parse_error = TRUE, options = NULL)
```

## Arguments

- text:

  String to parse. Use either `file` or `text`, but not both.

- ranges:

  Can be used to parse part(s) of the input. It must be a data frame
  with integer columns `start_row`, `start_col`, `end_row`, `end_col`,
  `start_byte`, `end_byte`, in this order.

- fail_on_parse_error:

  Logical, whether to error if there are parse errors in the document.
  Default is `TRUE`.

- options:

  Named list of parsing options, see [tsjsonc
  options](https://gaborcsardi.github.io/tsjsonc/reference/tsjsonc_options.md).

- file:

  Path of a file to parse. Use either `file` or `text`, but not both.

## Value

A ts_tree_jsonc object.

## Details

ts_tree_jsonc objects have
[`format()`](https://rdrr.io/r/base/format.html) and
[`print()`](https://rdrr.io/r/base/print.html) methods to pretty-print
them to the screen.

They can be converted to a data frame using the [single
bracket](https://r-lib.github.io/tsitter/reference/ts_tree-brackets.html)
operator.

## See also

[`tsitter::ts_tree_select()`](https://r-lib.github.io/tsitter/reference/ts_tree_select.html)
to select part(s) of a ts_tree_jsonc object,
[`tsitter::ts_tree_unserialize()`](https://r-lib.github.io/tsitter/reference/ts_tree_unserialize.html)
to extract the selected part(s),
[`tsitter::ts_tree_format()`](https://r-lib.github.io/tsitter/reference/ts_tree_format.html)
to format the selected part(s),
[`tsitter::ts_tree_delete()`](https://r-lib.github.io/tsitter/reference/ts_tree_delete.html),
[`tsitter::ts_tree_insert()`](https://r-lib.github.io/tsitter/reference/ts_tree_insert.html)
and
[`tsitter::ts_tree_update()`](https://r-lib.github.io/tsitter/reference/ts_tree_update.html)
to manipulate it.
[`tsitter::ts_tree_write()`](https://r-lib.github.io/tsitter/reference/ts_tree_write.html)
to save the JSON document to a file.

## Examples

``` r
library(tsitter)
text <- '
{
  "a": 1,
  "b": [2, 3, 4],
  "[r]": {
    "this": "setting",
    // A comment!
    "that": true
  }
}
'

# Parse the JSON, allowing comments (i.e. JSONC)
ts_parse_jsonc(text)
#> # jsonc (10 lines)
#>  1 | 
#>  2 | {
#>  3 |   "a": 1,
#>  4 |   "b": [2, 3, 4],
#>  5 |   "[r]": {
#>  6 |     "this": "setting",
#>  7 |     // A comment!
#>  8 |     "that": true
#>  9 |   }
#> 10 | }

# Try to parse the JSON, but comments aren't allowed!
try(ts_parse_jsonc(text, options = list(allow_comments = FALSE)))
#> Error in ts_tree_new.ts_language_jsonc(language = ts_language_jsonc(),  : 
#>   The JSON document contains comments, and this is not allowed. To allow comments, set the `allow_comments` option to `TRUE`.

# Extract parts of the JSON
ts_parse_jsonc(text) |>
  ts_tree_select("b") |>
  ts_tree_unserialize()
#> [[1]]
#> [[1]][[1]]
#> [1] 2
#> 
#> [[1]][[2]]
#> [1] 3
#> 
#> [[1]][[3]]
#> [1] 4
#> 
#> 
ts_parse_jsonc(text) |>
  ts_tree_select("[r]") |>
  ts_tree_unserialize()
#> [[1]]
#> [[1]]$this
#> [1] "setting"
#> 
#> [[1]]$that
#> [1] TRUE
#> 
#> 
ts_parse_jsonc(text) |>
  ts_tree_select("[r]", "that") |>
  ts_tree_unserialize()
#> [[1]]
#> [1] TRUE
#> 

# Use a `list()` combining strings and positional indices when
# arrays are involved
ts_parse_jsonc(text) |>
  ts_tree_select("b", 2) |>
  ts_tree_unserialize()
#> [[1]]
#> [1] 3
#> 
```
