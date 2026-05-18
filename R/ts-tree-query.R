#' Run tree-sitter queries on JSONC tree-sitter trees
#'
#' @usage
#' \method{ts_tree_query}{ts_tree_jsonc}(tree, query)
#'
#' @description
#' \eval{tsitter:::doc_insert("ts_tree_query_description", "tsjsonc")}
#'
#' @details
#' \eval{tsitter:::doc_insert("ts_tree_query_details", "tsjsonc")}
#' \eval{tsitter:::doc_extra()}
#'
#' @ts ts_tree_query_details_examples
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }'
#' )
#' json |> ts_tree_query("(number) @number")
#' ```
#'
#' @param tree
#' \eval{tsitter:::doc_insert("ts_tree_query_param_tree", "tsjsonc")}
#' @param query
#' \eval{tsitter:::doc_insert("ts_tree_query_param_query", "tsjsonc")}
#'
#' @return \eval{tsitter:::doc_insert("ts_tree_query_return", "tsjsonc")}
#'
#' @export
#' @examples
#' # Select all numbers in a JSONC document ------------------------------------
#' library(tsitter)
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }'
#' )
#' json |> ts_tree_query("(number) @number")

ts_tree_query.ts_tree_jsonc <- function(tree, query) {
  NextMethod()
}
