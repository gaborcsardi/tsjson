#' Serialize an R object to JSON
#'
#' Create JSON from an R object.
#' Note that this function is not a generic serializer that can represent
#' any R object in JSON. Also, you cannot expect that [unserialize_json()]
#' will do the exact inverse of [serialize_json()].
#'
#' tsjson functions [update_selected()] and [insert_into_selected()] use
#' [serialize_json()] to create new JSON code.
#'
#' See the examples below on how to create all possible JSON elements with
#' [serialize_json()].
#'
#' @param obj R object to serialize.
#' @param file If not `NULL` then the result if written to this file.
#' @param collapse If `file` is `NULL` then whether to return a character
#'   scalar or a character vector.
#' @inheritParams format_selected
#' @return If `file` is `NULL` then a character scalar (`collapse` = TRUE)
#'   or vector (`collapse` = FALSE). If `file` is not `NULL` then nothing.
#'
#' @export
#' @seealso [unserialize_json()] for the opposite.
#' @examples
#' # null
#' serialize_json(NULL)
#'
#' # true, false, use a logical scalar
#' serialize_json(TRUE)
#' serialize_json(FALSE)
#'
#' # strings, use a character scalar
#' serialize_json("string with escapes: \b \ud020")
#'
#' # number, use a numeric scalar
#' serialize_json(42.25)
#'
#' # array, use an unnamed list, i.e. _not_ an atomic vector
#' txt <- serialize_json(list(1, 2, 3,"x", "y"))
#' load_json(text = txt)
#'
#' # empty array
#' serialize_json(list())
#'
#' # object, use a named (or partially named) list, i.e. _not_ an atomic vector
#' txt <- serialize_json(list(a = 1, b = 2))
#' load_json(text = txt)
#'
#' # empty object, use a named empty list
#' serialize_json(structure(list(), names = character()))

serialize_json <- function(
  obj,
  file = NULL,
  collapse = FALSE,
  options = NULL
) {
  if (!missing(options)) {
    check_named_arg(options)
  }
  options <- as_tsjson_options(options)
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
