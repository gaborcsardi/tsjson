`%||%` <- function(l, r) {
  if (is.null(l)) {
    r
  } else {
    l
  }
}

map_int <- function(.x, .f, ...) {
  vapply(.x, .f, integer(1), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

na_omit <- function(x) {
  x[!is.na(x)]
}

middle <- function(x) {
  if (length(x) <= 2) {
    x[numeric()]
  } else {
    x[-c(1, length(x))]
  }
}
