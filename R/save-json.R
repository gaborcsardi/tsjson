#' Write a tsjson object to a file
#'
#' @param json tsjson object.
#' @param file File or connection to write to. Both binary and text
#'   connections are supported. Use `stdout()` to output to the screen.
#' @return Nothing.
#'
#' @seealso [load_json()] to create a tsjson object from a JSON file or
#'   string. [unserialize_selected()] obtain a JSON string for a tsjson
#'   object.
#' @export

save_json <- function(json, file = NULL) {
  file <- file %||% attr(json, "file")
  if (is.null(file)) {
    stop(cnd(
      "Don't know which file to save JSON document to. You need to \\
       specify the `file` argument."
    ))
  }

  text <- attr(json, "text")
  if (length(text) > 0 && text[length(text)] != 0xa) {
    text <- c(text, as.raw(0xa))
  }
  if (inherits(file, "connection")) {
    if (summary(file)$mode == "wb") {
      writeBin(text, con = file)
    } else {
      cat(rawToChar(text), file = file)
    }
  } else {
    writeBin(text, con = file)
  }
  invisible()
}
