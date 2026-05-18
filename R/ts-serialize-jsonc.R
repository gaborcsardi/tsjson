#' Serialize an R object to JSON
#'
#' Create JSON from an R object.
#' Note that this function is not a generic serializer that can represent
#' any R object in JSON. Also, you cannot expect that [ts_unserialize_jsonc()]
#' will do the exact inverse of [ts_serialize_jsonc()].
#'
#' tsjsonc functions [ts_tree_update()] and [ts_tree_insert()] use
#' [ts_serialize_jsonc()] to create new JSON code.
#'
#' See the examples below on how to create all possible JSON elements with
#' [ts_serialize_jsonc()].
#'
#' @param obj R object to serialize.
#' @param file If not `NULL` then the result if written to this file.
#' @param collapse If `file` is `NULL` then whether to return a character
#'   scalar or a character vector.
#' @inheritParams tsitter::ts_tree_format
#' @return If `file` is `NULL` then a character scalar (`collapse` = TRUE)
#'   or vector (`collapse` = FALSE). If `file` is not `NULL` then nothing.
#'
#' @export
#' @seealso [ts_unserialize_jsonc()] for the opposite.
#' @examples
#' library(tsitter)
#' # null
#' ts_serialize_jsonc(NULL)
#'
#' # true, false, use a logical scalar
#' ts_serialize_jsonc(TRUE)
#' ts_serialize_jsonc(FALSE)
#'
#' # strings, use a character scalar
#' ts_serialize_jsonc("string with escapes: \b \ud020")
#'
#' # number, use a numeric scalar
#' ts_serialize_jsonc(42.25)
#'
#' # array, use an unnamed list, i.e. _not_ an atomic vector
#' txt <- ts_serialize_jsonc(list(1, 2, 3,"x", "y"))
#' ts_parse_jsonc(txt)
#'
#' # empty array
#' ts_serialize_jsonc(list())
#'
#' # object, use a named (or partially named) list, i.e. _not_ an atomic vector
#' txt <- ts_serialize_jsonc(list(a = 1, b = 2))
#' ts_parse_jsonc(txt)
#'
#' # empty object, use a named empty list
#' ts_serialize_jsonc(structure(list(), names = character()))

ts_serialize_jsonc <- function(
  obj,
  file = NULL,
  collapse = FALSE,
  options = NULL
) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tsjsonc_options(options)
  opts <- list(auto_unbox = TRUE, format = options[["format"]])
  if (is.null(file)) {
    if (collapse) {
      tojson$write_str(obj, opts = opts)
    } else {
      tojson$write_lines(obj, opts = opts)
    }
  } else {
    tojson$write_file(obj, file, opts = opts)
  }
}
