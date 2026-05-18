#' Parse a JSON file or string into a ts_tree_jsonc object
#'
#' Parse a JSON file or string and create a ts_tree_jsonc object that
#' represents its document. This object can then be queried and
#' manipulated.
#'
# TODO: add manual page and link it here
#' ts_tree_jsonc objects have `format()` and `print()` methods to pretty-print
#' them to the screen.
#'
#' They can be converted to a data frame using the
#' [single bracket][tsitter::ts_tree-brackets] operator.
#'
# TODO: do not inherit from other package
#' @inheritParams tsitter::ts_tree_new
#' @param options Named list of parsing options, see
#'   [tsjsonc options][tsjsonc_options].
#' @return A ts_tree_jsonc object.
#'
#' @seealso [ts_tree_select()] to select part(s) of a ts_tree_jsonc
#'   object, [ts_tree_unserialize()] to extract the selected part(s),
#'   [ts_tree_format()] to format the selected part(s),
#'   [ts_tree_delete()], [ts_tree_insert()] and
#'   [ts_tree_update()] to manipulate it. [ts_tree_write()] to save
#'   the JSON document to a file.
#' @export
#' @examples
#' library(tsitter)
#' text <- '
#' {
#'   "a": 1,
#'   "b": [2, 3, 4],
#'   "[r]": {
#'     "this": "setting",
#'     // A comment!
#'     "that": true
#'   }
#' }
#' '
#'
#' # Parse the JSON, allowing comments (i.e. JSONC)
#' ts_parse_jsonc(text)
#'
#' # Try to parse the JSON, but comments aren't allowed!
#' try(ts_parse_jsonc(text, options = list(allow_comments = FALSE)))
#'
#' # Extract parts of the JSON
#' ts_parse_jsonc(text) |>
#'   ts_tree_select("b") |>
#'   ts_tree_unserialize()
#' ts_parse_jsonc(text) |>
#'   ts_tree_select("[r]") |>
#'   ts_tree_unserialize()
#' ts_parse_jsonc(text) |>
#'   ts_tree_select("[r]", "that") |>
#'   ts_tree_unserialize()
#'
#' # Use a `list()` combining strings and positional indices when
#' # arrays are involved
#' ts_parse_jsonc(text) |>
#'   ts_tree_select("b", 2) |>
#'   ts_tree_unserialize()

ts_parse_jsonc <- function(
  text,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  options = NULL
) {
  text <- as_character(text)
  ts_tree_new(
    language = ts_language_jsonc(),
    file = NULL,
    text = text,
    ranges = ranges,
    fail_on_parse_error = fail_on_parse_error,
    options = options
  )
}

#' @rdname ts_parse_jsonc
#' @export

ts_read_jsonc <- function(
  file,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  options = NULL
) {
  file <- as_existing_file(file)
  ts_tree_new(
    language = ts_language_jsonc(),
    text = NULL,
    file = file,
    ranges = ranges,
    fail_on_parse_error = fail_on_parse_error,
    options = options
  )
}
