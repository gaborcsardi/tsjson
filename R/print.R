#' @export

print.tsjson <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.tsjson <- function(x, ...) {
  lns <- strsplit(rawToChar(attr(x, "text")), "\r?\n")[[1]]
  num <- cli::col_grey(format(seq_along(lns)))
  mark <- rep("", length(lns))

  sel <- attr(x, "selection")
  if (length(sel) > 0) {
    rows <- sort(unique(unlist(lapply(sel, function(s) {
      x$start_row[s]:x$end_row[s] + 1L
    }))))
    lns[rows] <- cli::col_cyan(lns[rows])
    mark[] <- " "
    mark[rows] <- cli::bg_cyan(">")
  }

  c(
    "<json>",
    paste0((mark), " ", num, if (length(lns)) cli::col_grey(" | "), lns)
  )
}
