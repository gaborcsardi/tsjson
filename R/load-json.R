#' Parse a JSON file or string into a tsjson object
#'
#' TODO
#'
#' @inheritParams sexpr_json
#' @return A tsjson object.
#' @export

load_json <- function(file = NULL, text = NULL, ranges = NULL) {
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
  tt <- token_table(text = text, ranges = ranges)

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
#' TODO
#'
#' @param x tsjson object.
#' @param i,j indices.
#' @param drop Ignored.
#' @export

`[.tsjson` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "tsjson")
  requireNamespace("pillar", quietly = TRUE)
  NextMethod("[")
}
