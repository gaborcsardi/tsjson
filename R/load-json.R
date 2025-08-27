#' @export

load_json <- function(file = NULL, text = NULL) {
  if (is.null(text) + is.null(file) != 1) {
    stop(
      "Invalid arguments in `load_json()`: exactly one of ",
      "`file` and `text` must be given."
    )
  }
  if (is.null(text)) {
    text <- readBin(file, "raw", n = file.size(file))
  }
  if (is.character(text)) {
    text <- charToRaw(paste(text, collapse = "\n"))
  }
  # TODO: error on error, get error position
  tt <- token_table(text = text)
  attr(tt, "text") <- text
  class(tt) <- c("tsjson", class(tt))
  tt
}

#' @export

print.tsjson <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.tsjson <- function(x, ...) {
  c(
    "<json>",
    strsplit(rawToChar(attr(x, "text")), "\r?\n")[[1]]
  )
}
