# Create a new tree-sitter tree for a JSONC document

The result is a `ts_tree` object. A `ts_tree` object may be queried,
edited, formatted, written to file, etc. using `ts_tree` methods.

## Usage

``` r
# S3 method for class 'ts_language_jsonc'
ts_tree_new(
  language,
  file = NULL,
  text = NULL,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  options = NULL,
  ...
)
```

## Arguments

- language:

  Language of the file or string, a `ts_language` object,e.g. the return
  value of
  [`tsjsonc::ts_language_jsonc()`](https://gaborcsardi.github.io/tsjsonc/reference/ts_language_jsonc.md).

- file:

  Path of a file to parse. Use either `file` or `text`, but not both.

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

  Named list of formatting options, see [tsjsonc
  options](https://gaborcsardi.github.io/tsjsonc/reference/tsjsonc_options.md).

- ...:

  Reserved for future use.

## Value

A `ts_tree` object representing the parse tree of the input. You can use
the single bracket `` `[` `` operator to convert it to a data frame.

## Details

 

    jsonc <- tsitter::ts_tree_new(
      tsjsonc::ts_language_jsonc(),
      text = "{ \"a\": true, // comment\n \"b\": [1, 2, 3], }"
    )
    jsonc


    #> # jsonc (2 lines)
    #> 1 | { "a": true, // comment
    #> 2 |  "b": [1, 2, 3], }
