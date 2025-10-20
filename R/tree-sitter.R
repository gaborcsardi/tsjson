#' Show the syntax tree structure of a JSON file or string
#'
#' @inheritParams token_table
#'
#' @export
#' @examples
#' sexpr_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")

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

#' Get the token table of a JSON file or string
#'
#' @param file Path of a JSON file. Use either `file` or `text`.
#' @param text JSON string. Use either `file` or `text`.
#' @param ranges Can be used to parse part(s) of the input. It must be a
#'   data frame with integer columns `start_row`, `start_col`, `end_row`,
#'   `end_col`, `start_byte`, `end_byte`, in this order.
#' @param options List of options, see [tsjson_options()]. This argument
#'   must be named and cannot be abbreviated.
#' @param fail_on_parse_error Logical, whether to error if there are
#'   parse errors in the JSON document. Default is `TRUE`.
#'
#' @return A data frame with one row per token, and columns:
#' * `id`: integer, the id of the token.
#' * `parent`: integer, the id of the parent token. The root token has
#'   parent `NA`
#' * `field_name`: character, the field name of the token in its parent.
#' * `type`: character, the type of the token.
#' * `code`: character, the actual code of the token.
#' * `start_byte`, `end_byte`: integer, the byte positions of the token
#'   in the input.
#' * `start_row`, `start_column`, `end_row`, `end_column`: integer, the
#'   position of the token in the input.
#' * `is_missing`: logical, whether the token is a missing token added by
#'   the parser to recover from errors.
#' * `has_error`: logical, whether the token has a parse error.
#' * `children`: list of integer vectors, the ids of the children tokens.
#'
#' @export
#' @examples
#' token_table(text = "{ \"a\": true, \"b\": [1, 2, 3] }")

token_table <- function(
  file = NULL,
  text = NULL,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  options = NULL
) {
  if (!missing(options)) {
    check_named_arg(options)
  }
  options <- as_tsjson_options(options)
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

  if (fail_on_parse_error && (tab$has_error[1] || any(tab$is_missing))) {
    stop(tsjson_parse_error_cnd(table = tab, text = text))
  }

  if (!options[["allow_comments"]]) {
    comments <- which(tab$type == "comment")
    if (length(comments) > 0) {
      stop(cnd(
        "The JSON document contains comments, and this is not allowed. \\
         To allow comments, set the `allow_comments` option to `TRUE`."
      ))
    }
  }

  top <- tab$children[[1]]
  top <- top[tab$type[top] != "comment"]
  if (!options[["allow_empty_content"]] && length(top) == 0) {
    stop(cnd(
      "The JSON document is empty, and this is not allowed. \\
       To allow this, set the `allow_empty_content` option to `TRUE`."
    ))
  }

  tab
}

#' Show the annotated syntax tree of a JSON file or string
#' @inheritParams token_table
#' @export
#' @examples
#' syntax_tree_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")

syntax_tree_json <- function(
  file = NULL,
  text = NULL,
  ranges = NULL,
  options = NULL
) {
  if (!missing(options)) {
    check_named_arg(options)
  }
  options <- as_tsjson_options(options)
  tokens <- token_table(file, ranges = ranges, text = text, options = options)

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
#' See https://tree-sitter.github.io/tree-sitter/ on writing tree-sitter
#' queries.
#'
#' @param query Character string, the tree-sitter query to run.
#' @inheritParams token_table
#' @return A list with entries `patterns` and `matched_captures`.
#'   `patterns` contains information about all patterns in the queries and
#'   it is a data frame with columns: `id`, `name`, `pattern`, `match_count`.
#'   `matched_captures` contains information about all matches, and it has
#'   columns `id`, `pattern`, `match`, `start_byte`, `end_byte`, `start_row`,
#'   `start_column`, `end_row`, `end_column`, `name`, `code`. The `pattern`
#'   column of `matched_captured` refers to the `id` column of `patterns`.
#'
#' @export
#' @examples
#' # A very simple JSON document
#' txt <- "{ \"a\": 1, \"b\": \"foo\", \"c\": 20 }"
#'
#' # Take a look at it
#' load_json(text = txt) |> format_selected()
#'
#' # Select all pairs where the value is a number
#' query_json(text = txt, query = "((pair value: (number) @num))")

query_json <- function(
  file = NULL,
  text = NULL,
  query,
  ranges = NULL
) {
  if (is.null(text) + is.null(file) != 1) {
    stop(cnd(
      "Invalid arguments in `query_json()`: exactly one of `file` \\
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
