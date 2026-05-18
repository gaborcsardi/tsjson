#' Replace selected JSON elements with a new element
#'
#' Replace all selected elements with a new element. If `tree` has no
#' selection then the whole document is replaced.
#'
#' @ts ts_tree_update_details_no_selection
#'
#' ```{asciicast}
#' tree <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' tree |> ts_tree_update(as.list(4:6))
#' ```
#'
#' @ts ts_tree_update_details_empty_selection
#'
#' ```{asciicast}
#' tree <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' tree |> ts_tree_select("new") |> ts_tree_update(as.list(4:6))
#' ```
#'
#' @param tree ts_tree_jsonc object.
#' @param new R object that will be serialized to JSON (using
#'   [ts_serialize_jsonc()]) and inserted in place of the selected JSON
#'   elements.
#' @param options Named list of formatting options, see
#'   [tsjsonc options][tsjsonc_options].
#' @param ... Reserved for future use.
#' @return The updated ts_tree_jsonc object
#'
#' @export
#' @keywords internal
#'
#' @examples
#' library(tsitter)
#' tree <- ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' tree
#'
#' tree |> ts_tree_select("a") |> ts_tree_update(list("new", "element"))

ts_tree_update.ts_tree_jsonc <- function(tree, new, options = NULL, ...) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tsjsonc_options(options)
  # TODO: check that ... is empty
  selection <- ts_tree_selection(tree)
  ptr <- length(selection)
  select <- selection[[ptr]]$nodes

  # if no selection, then maybe this is an insert
  if (length(select) == 0) {
    while (length(selection[[ptr]]$nodes) == 0) {
      slt <- selection[[ptr]]$selector
      # only if characters
      if (inherits(slt, "ts_tree_selector") || !is.character(slt)) {
        return(tree)
      }
      ptr <- ptr - 1L
      new <- structure(
        replicate(length(slt), new, simplify = FALSE),
        names = slt
      )
    }
    attr(tree, "selection") <- selection[1:ptr]
    return(ts_tree_insert(tree, new[[1]], key = names(new)))
  }

  fmt <- replicate(
    length(select),
    ts_serialize_jsonc(new, collapse = FALSE, options = options),
    simplify = FALSE
  )

  # keep original indentation at the start row
  for (i in seq_along(select)) {
    sel1 <- select[i]
    prevline <- rev(which(tree$end_row == tree$start_row[sel1] - 1))[1]
    ind0 <- sub("^.*\n", "", tree$tws[prevline])
    if (!is.na(prevline)) {
      fmt[[i]] <- paste0(c("", rep(ind0, length(fmt[[i]]) - 1L)), fmt[[i]])
    }
  }

  subtrees <- lapply(select, get_subtree, tree = tree, with_root = FALSE)
  deleted <- unique(unlist(subtrees))

  # need to keep the trailing ws of the last element
  lasts <- map_int(subtrees, max_or_na)
  tws <- tree$tws[lasts]
  tree$code[deleted] <- NA_character_
  tree$tws[deleted] <- NA_character_

  # keep select nodes to inject the new elements
  tree$code[select] <- paste0(
    map_chr(fmt, paste, collapse = "\n"),
    ifelse(is.na(tws), "", tws)
  )
  tree$tws[select] <- NA_character_

  parts <- c(rbind(tree$code, tree$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- ts_parse_jsonc(text = text)
  attr(new, "file") <- attr(tree, "file")

  new
}
