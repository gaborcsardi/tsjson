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
}
