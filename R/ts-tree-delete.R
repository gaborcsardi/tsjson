#' Delete selected elements from a ts_tree_jsonc object
#'
#' @ts ts_tree_delete_details_formatting JSONC example
#'
#' ```{asciicast}
#' jsonc <- tsjsonc::ts_parse_jsonc(
#'   "{ \"a\": true, \"b\": [1, 2, 3] }"
#' ) |>
#'   tsitter::ts_tree_format()
#' jsonc
#' ```
#'
#' ```{asciicast}
#' jsonc |> ts_tree_select("a") |> ts_tree_delete()
#' ```
#'
#' @ts ts_tree_delete_details_no_selection JSONC example
#'
#' ```{asciicast}
#' jsonc <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' jsonc |> ts_tree_delete()
#' ```
#'
#' @ts ts_tree_delete_details_empty_selection
#'
#' ```{asciicast}
#' jsonc <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' jsonc |> ts_tree_select("c") |> ts_tree_delete()
#' ```
#'
#' @ts ts_tree_delete_details_comments
#'
#' ```{asciicast}
#' jsonc <- tsjsonc::ts_parse_jsonc(
#'   "// top comment\n{ \"a\": // comment\n  true,\n \"b\": [1, 2, 3] }"
#' ) |> tsitter::ts_tree_format()
#' jsonc
#' ```
#'
#' ```{asciicast}
#' jsonc |> ts_tree_select("a") |> ts_tree_delete()
#' ```
#'
#' @details
#' \eval{tsitter:::doc_insert("ts_tree_delete_details", "tsjsonc")}
#'
#' Comments appearing inside the deleted elements are also deleted. Other
#' comments are left as is.
#'
#' @usage
#' \method{ts_tree_delete}{ts_tree_jsonc}(tree, ...)
#' @param tree
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_delete_param_tree", "tsjsonc")}
#' @param ... Reserved for future use.
#' @return
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_delete_return", "tsjsonc")}
#'
#' @export
#' @examples
#' library(tsitter)
#' tree <- ts_parse_jsonc("{ \"a\": //comment\ntrue, \"b\": [1, 2, 3] }")
#' tree
#'
#' tree |> ts_tree_select("a")
#' tree |> ts_tree_select("a") |> ts_tree_delete()

ts_tree_delete.ts_tree_jsonc <- function(tree, ...) {
  # TODO: check that ... is empty
  select <- ts_tree_selected_nodes(tree)

  if (length(select) == 0) {
    attr(tree, "selection") <- NULL
    return(tree)
  }

  # if deleting from an array, then we need to look at the array and
  # remove some commas, probably
  pares <- tree$parent[select]
  ptypes <- tree$type[pares]
  trimmed_arrays <- unique(pares[ptypes == "array"])
  trimmed_pairs <- pares[ptypes == "pair"]
  select <- c(select, trimmed_pairs)
  trimmed_objects <- unique(tree$parent[trimmed_pairs])

  deleted <- select
  while (TRUE) {
    deleted2 <- unique(c(deleted, unlist(tree$children[deleted])))
    if (length(deleted2) == length(deleted)) {
      break
    }
    deleted <- deleted2
  }

  # TODO: when should we leave trailing whitespace? E.g. when removing
  # a trailing comma?
  trim_commas <- function(id, open, close) {
    # remove deleted children and see what is left
    allchld <- tree$children[[id]]
    chld <- setdiff(allchld, deleted)
    nc <- length(chld)
    # if nothing left, then nothing to do
    if (nc == 2) {
      return()
    }
    ctypes <- tree$type[chld]
    todel <- rep(FALSE, length(chld))
    # this is hard to write in a vectorized form, it is easier iteratively
    # leading commas
    for (i in seq_along(todel)[-1]) {
      if (ctypes[i] == ',') {
        todel[i] <- TRUE
      } else {
        break
      }
    }
    # trailing commas
    for (i in rev(seq_along(todel))[-1]) {
      if (ctypes[i] == ',') {
        todel[i] <- TRUE
      } else {
        break
      }
    }
    # duplicate commas
    todel[ctypes[-nc] == ',' & ctypes[-1] == ','] <- TRUE
    chdel <- chld[which(todel)]
    deleted <<- c(deleted, chdel)

    # if the last element is deleted, take the trailing whitespace of
    # its last token and add it to the last token of the last kept element
    chdeld <- intersect(allchld, deleted)
    chkept <- setdiff(allchld, chdeld)
    last_deld <- chdeld[length(chdeld)]
    last_kept <- chkept[length(chkept) - 1L]
    if (last_deld > last_kept) {
      while (is.na(tree$code[last_deld])) {
        last_deld <- tail(tree$children[[last_deld]], 1)
      }
      while (is.na(tree$code[last_kept])) {
        last_kept <- tail(tree$children[[last_kept]], 1)
      }
      tree$tws[last_kept] <<- tree$tws[last_deld]
    }
  }

  # trim commas from arrays
  trimmed_arrays <- setdiff(trimmed_arrays, deleted)
  for (arr in trimmed_arrays) {
    trim_commas(arr, "[", "]")
  }

  # trim pairs and commas from objects
  trimmed_objects <- setdiff(trimmed_objects, deleted)
  for (obj in trimmed_objects) {
    trim_commas(obj, "{", "}")
  }

  tree2 <- tree[-deleted, ]

  # update text
  parts <- c(rbind(tree2$code, tree2$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- ts_parse_jsonc(text = text)
  attr(new, "file") <- attr(tree, "file")

  new
}
