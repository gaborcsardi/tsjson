#' Serialize an R object to JSON
#'
#' TODO
#'
#' @param obj R object to serialize.
#' @param file If not `NULL` then the result if written to this file.
#' @param collapse If `file` is `NULL` then whether to return a character
#'   scalar or a character vector.
#' @param format Formatting, one of:
#'   - `"pretty"`: arrays and objects are formatted in multiple lines,
#'   - `"compact"`: format everything without whitespace,
#'   - `"oneline"`: format everything without newlines, but include
#'     whitespace after commas, colons, opening brackets and braces, and
#'     before closing brackets and braces.
#' @return If `file` is `NULL` then a character scalar (`collapse` = TRUE)
#'   or vector (`collapse` = FALSE). If `file` is not `NULL` then nothing.
#'
#' @export

serialize_json <- function(
  obj,
  file = NULL,
  collapse = FALSE,
  format = c("pretty", "compact", "oneline")
) {
  format <- match.arg(format)
  opts <- list(auto_unbox = TRUE, format = format)
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
