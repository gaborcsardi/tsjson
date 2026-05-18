#' Unserialize a JSON file or string into an R object
#'
#' The purpose of this function is to convert a JSON file or string into
#' an R object reliably.
#'
#' See examples below on how the different JSON elements are mapped to
#' R objects.
#'
#' @inheritParams tsitter::ts_tree_new
#' @param options Named list of parsing options, see
#'   [tsjsonc options][tsjsonc_options].
#' @return R object.
#'
#' @export
#' @seealso [ts_serialize_jsonc()] for the opposite, [ts_tree_select()] and
#' [ts_tree_unserialize()] to unserialize part(s) of a JSON document.
#' [tsitter::ts_tree_new()] to load a JSON document and then manipulate it.
#' @examples
#' library(tsitter)
#' # null -> NULL
#' ts_unserialize_jsonc(text = "null")
#'
#' # true, false -> TRUE, FALSE
#' ts_unserialize_jsonc(text = "true")
#' ts_unserialize_jsonc(text = "false")
#'
#' # string -> character scalar
#' ts_unserialize_jsonc(text = "\"string with escapes: \\b \\ud020\"")
#'
#' # number -> double scalar
#' ts_unserialize_jsonc(text = "42.25")
#'
#' # array -> unnamed list
#' ts_unserialize_jsonc(text = "[1, 2, 3]")
#'
#' # object -> named list
#' ts_unserialize_jsonc(text = "{\"a\": 1, \"b\": 2 }")

ts_unserialize_jsonc <- function(
  file = NULL,
  text = NULL,
  ranges = NULL,
  options = NULL
) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tsjsonc_options(options)
  # parse file/text
  tree <- ts_tree_new(
    language = ts_language_jsonc(),
    file = file,
    text = text,
    ranges = ranges,
    options = options
  )

  # this uses the default selection
  unserialize_element(tree, ts_tree_selected_nodes(tree))
}
