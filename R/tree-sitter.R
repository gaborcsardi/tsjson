#' Show the syntax tree structure of a JSON file or string
#'
#' TODO
#'
#' @param file Path of a JSON file. Use either `file` or `text`.
#' @param text JSON string. Use either `file` or `text`.
#' @param ranges Optionally a list of integer pairs defining ranges within
#'   `file` or `text` to parse.
#'
#' @export

sexpr_json <- function(
  file = NULL,
  text = NULL,
  ranges = NULL
) {
  if (is.null(text) + is.null(file) != 1) {
    stop(cnd(
      "Invalid arguments in `sexpr_json()`: exactly one of `file` \\
       and `text` must be given."
    ))
  }
  if (is.null(text)) {
    text <- readBin(file, "raw", n = file.size(file))
  }
  if (is.character(text)) {
    text <- charToRaw(paste(text, collapse = "\n"))
  }
  call_with_cleanup(c_s_expr, text, 0L, ranges)
}

token_table <- function(
  file = NULL,
  text = NULL,
  ranges = NULL
) {
  if (is.null(text) + is.null(file) != 1) {
    stop(cnd(
      "Invalid arguments in `token_table()`: exactly one of `file` \\
       and `text` must be given."
    ))
  }
  if (is.null(text)) {
    text <- readBin(file, "raw", n = file.size(file))
  }
  if (is.character(text)) {
    text <- charToRaw(paste(text, collapse = "\n"))
  }
  tab <- call_with_cleanup(c_token_table, text, 0L, ranges)
  lvls <- seq_len(nrow(tab))
  tab$children <- I(unname(split(lvls, factor(tab$parent, levels = lvls))))
  attr(tab, "file") <- file

  # this is a workarond for TS adding code to a non-terminal array/object node
  tab$code[tab$type %in% c("array", "object")] <- NA_character_

  tab
}

#' Show the annotated syntax tree of a JSON file or string
#' @inheritParams sexpr_json
#' @export

syntax_tree_json <- function(
  file = NULL,
  text = NULL,
  ranges = NULL
) {
  tokens <- token_table(file, ranges = ranges, text = text)

  type <- tokens$type
  fn <- attr(tokens, "file")
  if (cli::ansi_has_hyperlink_support() && !is.null(fn)) {
    type <- cli::style_hyperlink(
      type,
      sprintf(
        "file://%s:%d:%d",
        normalizePath(fn, mustWork = NA),
        tokens$start_row + 1L,
        tokens$start_column + 1
      )
    )
  }

  linum <- tokens$start_row + 1
  linum <- ifelse(duplicated(linum), "", as.character(linum))
  linum <- format(linum, justify = "right")
  # this is the spacer we need to put in for multi-line tokens
  nlspc <- paste0("\n\t", strrep(" ", nchar(linum[1])), "|")
  code <- ifelse(
    is.na(tokens$code),
    "",
    paste0(strrep(" ", tokens$start_column), tokens$code)
  )

  # we put in a \t, and later use it to align the lines vertically
  treetab <- data_frame(
    id = as.character(tokens$id),
    children = lapply(tokens$children, as.character),
    label = paste0(
      type,
      "\t",
      linum,
      "|",
      gsub("\n", nlspc, code, fixed = TRUE)
    )
  )
  tree <- cli::tree(treetab)

  # align lines vertically. the size of the alignment is measured
  # without the ANSI sequences, but then the substitution uses the
  # full ANSI string
  tabpos <- regexpr("\t", cli::ansi_strip(tree), fixed = TRUE)
  maxtab <- max(tabpos)
  tabpos2 <- regexpr("\t", tree, fixed = TRUE)
  regmatches(tree, tabpos2) <- strrep(" ", maxtab - tabpos + 4)

  tree
}

#' Run tree-sitter queries on a JSON file or string
#'
#' TODO
#'
#' @param query Character string, the tree-sitter query to run.
#' @inheritParams sexpr_json
#' @return A list with entries `patterns` and `matched_captures`.
#'   TODO
#'
#' @export

query_json <- function(
  file = NULL,
  text = NULL,
  query,
  ranges = NULL
) {
  if (is.null(text) + is.null(file) != 1) {
    stop(cnd(
      "Invalid arguments in `code_query()`: exactly one of `file` \\
       and `text` must be given."
    ))
  }

  qlen <- nchar(query, type = "bytes") + 1L # + \n
  qbeg <- c(1L, cumsum(qlen))
  qnms <- names(query) %||% rep(NA_character_, length(query))
  query1 <- paste0(query, "\n", collapse = "")

  if (!is.null(text)) {
    if (is.character(text)) {
      text <- charToRaw(paste(text, collapse = "\n"))
    }
    res <- call_with_cleanup(c_code_query, text, query1, 0L, ranges)
  } else {
    res <- call_with_cleanup(c_code_query_path, file, query1, 0L, ranges)
  }

  qorig <- as.integer(cut(res[[1]][[3]], breaks = qbeg, include.lowest = TRUE))

  list(
    patterns = data_frame(
      id = seq_along(res[[1]][[1]]),
      name = qnms[qorig],
      pattern = res[[1]][[1]],
      match_count = res[[1]][[2]]
    ),
    matched_captures = data_frame(
      id = map_int(res[[2]], "[[", 3L),
      pattern = map_int(res[[2]], "[[", 1L),
      match = map_int(res[[2]], "[[", 2L),
      start_byte = map_int(res[[2]], "[[", 6L),
      end_byte = map_int(res[[2]], "[[", 7L),
      start_row = map_int(res[[2]], "[[", 8L),
      start_column = map_int(res[[2]], "[[", 9L),
      end_row = map_int(res[[2]], "[[", 10L),
      end_column = map_int(res[[2]], "[[", 11L),
      name = map_chr(res[[2]], "[[", 4L),
      code = map_chr(res[[2]], "[[", 5L)
    )
  )
}
