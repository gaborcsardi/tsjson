#' @export

print.tsjson <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.tsjson <- function(x, ...) {
  lns <- strsplit(rawToChar(attr(x, "text")), "\r?\n")[[1]]
  num <- cli::col_grey(format(seq_along(lns)))
  mark <- rep(" ", length(lns))

  # do not highlight the default selection
  sel <- get_selected_nodes(x, default = FALSE)
  if (length(sel) > 0) {
    for (sel1 in sel) {
      rows <- x$start_row[sel1]:x$end_row[sel1] + 1L
      mark[rows] <- cli::bg_cyan(">")
      # one row only
      if (length(rows) == 1) {
        lns[rows] <- hl(
          lns[rows],
          x$start_column[sel1] + 1L,
          x$end_column[sel1]
        )
      } else {
        # first row
        lns[rows[1]] <- hl(lns[rows[1]], x$start_column[sel1] + 1L, end = NULL)
        # middle rows, if any
        if (length(rows) > 2) {
          mid <- rows[-c(1, length(rows))]
          lns[mid] <- hl(lns[mid])
        }
        # last row
        if (length(rows) >= 2) {
          lns[rows[length(rows)]] <- hl(
            lns[rows[length(rows)]],
            start = NULL,
            x$end_column[sel1]
          )
        }
      }
    }
  }

  c(
    "<json>",
    paste0((mark), " ", num, if (length(lns)) cli::col_grey(" | "), lns)
  )
}

# TODO: only vectorized for the default case

hl <- function(txt, start = NULL, end = NULL) {
  if (is.null(start) && is.null(end)) {
    cli::col_cyan(txt)
  } else if (is.null(end) && !is.null(start)) {
    stopifnot(length(txt) == 1)
    paste0(
      if (start > 1) {
        cli::ansi_substr(txt, 1, start - 1)
      },
      cli::col_cyan(cli::ansi_substr(txt, start, cli::ansi_nchar(txt)))
    )
  } else if (is.null(start) && !is.null(end)) {
    stopifnot(length(txt) == 1)
    nc <- cli::ansi_nchar(txt)
    paste0(
      cli::col_cyan(cli::ansi_substr(txt, 1, end)),
      if (end < nc) {
        cli::ansi_substr(txt, end + 1, nc)
      }
    )
  } else {
    stopifnot(length(txt) == 1)
    nc <- cli::ansi_nchar(txt)
    paste0(
      if (start > 1) {
        cli::ansi_substr(txt, 1, start - 1)
      },
      cli::col_cyan(cli::ansi_substr(txt, start, end)),
      if (end < nc) {
        cli::ansi_substr(txt, end + 1, nc)
      }
    )
  }
}
