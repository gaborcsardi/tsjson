#' @ts ts_tree_select_true JSONC example
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
#' )
#' json |> ts_tree_select(c("b", "c"), TRUE)
#' ```
#'
#' @ts ts_tree_select_character JSONC example
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
#' )
#' json |> ts_tree_select(c("a", "c"), c("c1"))
#' ```
#'
#' @ts ts_tree_select_integer JSONC
#'
#' <p>
#'
#' For JSONC positional indices can be used both for arrays and objects.
#' For other nodes nothing is selected.
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
#' )
#' json |> ts_tree_select(c("b", "c"), -1)
#' ```
#'
#' @ts ts_tree_select_regex JSONC example
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'  '{ "apple": 1, "almond": 2, "banana": 3, "cherry": 4 }'
#' )
#' json |> ts_tree_select(regex = "^a")
#' ```
#'
#' @ts ts_tree_select_tsquery JSONC
#'
#' See \code{\link[tsjsonc:ts_language_jsonc]{ts_language_jsonc()}} for
#' details on the JSONC grammar.
#'
#' <p>
#'
#' This example selects all numbers in the JSON document.
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }'
#' )
#' json |> ts_tree_select(query = "(number) @number")
#' ```
#'
#' @ts ts_tree_select_ids JSONC example
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
#' )
#' ts_tree_dom(json)
#' ```
#'
#' ```{asciicast}
#' json |> ts_tree_select(I(18))
#' ```
#'
#' @ts ts_tree_select_refine JSONC example
#'
#' ```{asciicast}
#' #| results = "hide"
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
#' )
#' json <- json |> ts_tree_select(c("b", "c"))
#' ```
#'
#' ```{asciicast}
#' json |> ts_tree_select(1:2)
#' ```
#'
#' ```{asciicast}
#' json |> ts_tree_select(1:2, refine = TRUE)
#' ```
#'
#' @ts ts_tree_select_set JSONC example
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
#' )
#' json
#' ```
#'
#' ```{asciicast}
#' json |> ts_tree_select("b", 1)
#' ```
#'
#' ```{asciicast}
#' ts_tree_select(json, "b", 1) <- 100
#' json
#' ```
#'
#' @ts ts_tree_select_brackets JSONC example
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
#' )
#' json |> ts_tree_select("b", 1)
#' ```
#'
#' ```{asciicast}
#' json[[list("b", 1)]]
#' ```
#'
#' @ts ts_tree_select_brackets_set JSONC example
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": null } }'
#' )
#' json
#' ```
#'
#' ```{asciicast}
#' json |> ts_tree_select("b", 1)
#' ```
#'
#' ```{asciicast}
#' json[[list("b", 1)]] <- 100
#' json
#' ```
#'
#' @ts ts_tree_select_examples JSONC examples
#'
#' ## Examples
#'
#' ```{asciicast}
#' library(tsitter)
#' json <- ts_parse_jsonc(ts_serialize_jsonc(list(
#'   a = list(a1 = list(1,2,3), a2 = "string"),
#'   b = list(4, 5, 6),
#'   c = list(c1 = list("a", "b"))
#' )))
#' ```
#'
#' ```{asciicast}
#' json
#' ```
#'
#' Select object by key:
#'
#' ```{asciicast}
#' json |> ts_tree_select("a")
#' ```
#'
#' Select within select, these are the same:
#'
#' ```{asciicast}
#' json |> ts_tree_select("a", "a1")
#' json |> ts_tree_select(list("a", "a1"))
#' ```
#'
#' Select elements of an array. All elements:
#'
#' ```{asciicast}
#' json |> ts_tree_select("b", TRUE)
#' ```
#'
#' First two elements:
#'
#' ```{asciicast}
#' json |> ts_tree_select("b", 1:2)
#' ```
#'
#' First and last elements:
#'
#' ```{asciicast}
#' json |> ts_tree_select("b", c(1, -1))
#' ```
#'
#' Regular expressions:
#'
#' ```{asciicast}
#' json |> ts_tree_select(c("a", "c"), regex = "1$")
#' ```
NULL

# TODO: keep the parse tree as an external pointer and reuse it.
# TODO: do we need to make sure that there is no recursive selection? Probably.

#' Update selected elements in a ts_tree_jsonc object
#'
#' Update the selected elements of a JSON document, using the replacement
#' function syntax.
#'
#' @param x,json ts_tree_jsonc object. Create a ts_tree_jsonc object with
#'   [tsitter::ts_tree_new()].
#' @param i,... Selectors, see [ts_tree_select()].
#' @param value New value. Will be serialized to JSON with
#'   [ts_serialize_jsonc()].
#' @return The updated ts_tree_jsonc object.
#'
#' @seealso Save the updated ts_tree_jsonc object to a file with
#'   [ts_tree_write()].
#'
#' @rdname select-set
#' @examples
#' library(tsitter)
#' json <- ts_parse_jsonc("{}")
#'
#' json <- json |>
#'   ts_tree_select("r", "editor.formatOnSave") |>
#'   ts_tree_update(TRUE)
#' json
#'
#' json <- json |>
#'   ts_tree_select("r", "editor.formatOnSave") |>
#'   ts_tree_delete()
#' json
#'
#' # Insert an array
#' json <- json |>
#'   ts_tree_select("foo") |>
#'   ts_tree_update(1:3)
#' json
#'
#' # Update the array at location 2
#' json |> ts_tree_select("foo", 2) |> ts_tree_update(0)
#'
#' # Insert at location 2
#' json |> ts_tree_select("foo") |> ts_tree_insert(0, at = 2)
#'
#' # Insert at the end of the array with `Inf` as `at`
#' json |> ts_tree_select("foo") |> ts_tree_insert(0, at = Inf)
#'
#' # Only the modified elements are reformatted
#' json <- ts_parse_jsonc('{"foo":[1,2],\n"bar":1}')
#' json |> ts_tree_select("foo") |> ts_tree_insert(0, at = Inf)
#'
#' # You can control how those elements are formatted
#' json |> ts_tree_select("foo") |>
#'   ts_tree_insert(0, at = Inf, options = list(indent_width = 2))
#' @name ts_tree_select<-
#' @keywords internal
NULL

#' @title
#' Select parts of a JSONC tree-sitter tree
#' @usage
#' \method{ts_tree_select}{ts_tree_jsonc}(tree, ..., refine = FALSE)
#' @param tree
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_select_param_tree", "tsjsonc")}
#' @param ... Reserved for future use.
#' @param refine
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_select_param_refine", "tsjsonc")}
#' @return
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_select_return", "tsjsonc")}
#'
#' @description
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_select_description", "tsjsonc")}
#'
#' This is the S3 method of the [tsitter::ts_tree_select()] generic,
#' for [ts_tree_jsonc][tsjsonc::ts_tree_jsonc] objects.
#'
#' @details
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_select_details", "tsjsonc")}
#' \eval{tsitter:::doc_insert("tsjsonc::ts_tree_select_examples", "tsjsonc")}
#' \eval{tsitter:::doc_extra()}
#' @export

ts_tree_select.ts_tree_jsonc <- function(tree, ..., refine = FALSE) {
  NextMethod()
}
