#' Parse a JSON file or string into a tsjson object
#'
#' Parse a JSON file or string and create a tsjson object that represents
#' its document. This object can then be queried and manipulated.
#'
#' tsjson objects have [`format()`][format.tsjson()] and
#' [`print()`][print.tsjson()] methods to pretty-print them to the screen.
#'
#' They can be converted to a data frame using the
#' [single bracket][tsjson-brackets] operator.
#'
#' @inheritParams token_table
#' @return A tsjson object.
#'
#' @seealso [select()] to select part(s) of a tsjson object,
#'   [unserialize_selected()] to extract the selected part(s),
#'   [format_selected()] to format the selected part(s),
#'   [delete_selected()], [insert_into_selected()] and
#'   [update_selected()] to manipulate it. [save_json()] to save
#'   the JSON document to a file.
#' @export
#' @examples
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
#' load_json(text = text)
#'
#' # Try to parse the JSON, but comments aren't allowed!
#' try(load_json(text = text, options = list(allow_comments = FALSE)))
#'
#' # Extract parts of the JSON
#' load_json(text = text) |> select("b") |> unserialize_selected()
#' load_json(text = text) |> select("[r]") |> unserialize_selected()
#' load_json(text = text) |> select("[r]", "that") |> unserialize_selected()
#'
#' # Use a `list()` combining strings and positional indices when
#' # arrays are involved
#' load_json(text = text) |> select("b", 2) |> unserialize_selected()

load_json <- function(
  file = NULL,
  text = NULL,
  ranges = NULL,
  options = NULL
) {
  if (!missing(options)) {
    check_named_arg(options)
  }
  options <- as_tsjson_options(options)
  if (is.null(text) + is.null(file) != 1) {
    stop(cnd(
      "Invalid arguments in `load_json()`: exactly one of `file` \\
       and `text` must be given."
    ))
  }
  if (is.null(text)) {
    text <- readBin(file, "raw", n = file.size(file))
  }
  if (is.character(text)) {
    text <- charToRaw(paste(text, collapse = "\n"))
  }
  # TODO: error on error, get error position
  tt <- token_table(text = text, ranges = ranges, options = options)

  # trailing whitespace for each token
  # first we add the leading whitespace to the document token
  # this way printing $code and $tws will print the whole document
  tt$tws <- rep("", nrow(tt))
  if ((lead <- tt$start_byte[1]) > 0) {
    tt$tws[1] <- rawToChar(text[1:lead])
  }

  # then the whitespace of the terminal nodes
  term <- which(!is.na(tt$code))
  from <- tt$end_byte[term] + 1L
  to <- c(tt$start_byte[term][-1], tt$end_byte[1])
  for (i in seq_along(term)) {
    if (from[i] <= to[i]) {
      tt$tws[term[i]] <- rawToChar(text[from[i]:to[i]])
    }
  }

  attr(tt, "text") <- text
  attr(tt, "file") <- if (!is.null(file)) normalizePath(file)
  class(tt) <- c("tsjson", class(tt))
  tt
}

#' Convert a tsjson object to a data frame
#'
#' Create a data frame for the syntax tree of a JSON document, by indexing
#' a tsjson object with single brackets. This is occasionally useful for
#' exploration and debugging.
#'
#' @param x tsjson object.
#' @param i,j indices.
#' @param drop Ignored.
#'
#' @name tsjson-brackets
#' @return A data frame with columns:
#'   `r doclist(colnames(load_json(text="")[]))`.
#' @export
#' @seealso [token_table()] to create the token table directly.
#'   Other JSON debugging tools: [sexpr_json()], [syntax_tree_json()],
#'   [query_json()]. [load_json()] for creating tsjson objects.
#' @examples
#' json <- load_json(text = serialize_json(list(
#'   a = list(a1 = list(1,2,3), a2 = "string"),
#'   b = list(4, 5, 6),
#'   c = list(c1 = list("a", "b"))
#' )))
#'
#' json
#'
#' json[]

`[.tsjson` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "tsjson")
  requireNamespace("pillar", quietly = TRUE)
  NextMethod("[")
}
