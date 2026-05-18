#' Format the selected JSON elements
#'
#' @ts ts_tree_format_details_no_selection
#'
#' ```{asciicast}
#' jsonc <- tsjsonc::ts_parse_jsonc("{ \"a\": [1,2,3] }")
#' jsonc |> ts_tree_format()
#' ```
#'
#' @ts ts_tree_format_details_empty_selection
#'
#' ```{asciicast}
#' jsonc <- tsjsonc::ts_parse_jsonc("{ \"a\": [1,2,3] }")
#' jsonc |> ts_tree_select("c") |> ts_tree_format()
#' ```
#'
#' @ts ts_tree_format_details_options
#'
#' ```{asciicast}
#' #| results = "hide"
#' jsonc <- tsjsonc::ts_parse_jsonc("{ \"a\": [1,2,3] }") |>
#'   ts_tree_format()
#' ```
#'
#' ```{asciicast}
#' jsonc |> ts_tree_select(TRUE) |>
#'   ts_tree_format(options = list(format = "oneline"))
#'
#' @details
#' If `tree` does not have a selection, then all of it is formatted.
#' If `tree` has an empty selection, then nothing happens.
#'
#' @param tree ts_tree_jsonc object.
#' @param options Named list of formatting options, see
#'   [tsjsonc options][tsjsonc_options].
#' @param ... Reserved for future use.
#' @return The updated ts_tree_jsonc object.
#'
#' @export
#' @keywords internal
#'
#' @examples
#' library(tsitter)
#' tree <- ts_parse_jsonc("{ \"a\": [1,2,3] }")
#' tree
#'
#' tree |> ts_tree_format()
#'
#' tree |> ts_tree_select("a") |> ts_tree_format()

ts_tree_format.ts_tree_jsonc <- function(
  tree,
  options = NULL,
  ...
) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tsjsonc_options(options)
  # TODO: check that ... is empty
  select <- ts_tree_selected_nodes(tree)
  fmt <- lapply(
    select,
    format_element,
    tree = tree,
    options = options
  )
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

get_subtree <- function(tree, id, with_root = FALSE) {
  sel <- c(if (with_root) id, tree$children[[id]])
  while (TRUE) {
    sel2 <- unique(c(sel, unlist(tree$children[sel])))
    if (length(sel2) == length(sel)) {
      return(sel)
    }
    sel <- sel2
  }
}

format_element <- function(tree, id, options) {
  switch(
    tree$type[id],
    null = {
      format_null(tree, id, options = options)
    },
    true = {
      format_true(tree, id, options = options)
    },
    false = {
      format_false(tree, id, options = options)
    },
    string = {
      format_string(tree, id, options = options)
    },
    number = {
      format_number(tree, id, options = options)
    },
    array = {
      format_array(tree, id, options = options)
    },
    object = {
      format_object(tree, id, options = options)
    },
    comment = {
      format_comment(tree, id, options = options)
    },
    pair = {
      format_pair(tree, id, options = options)
    },
    document = {
      format_document(tree, id, options = options)
    },
    "," = {
      format_comma(tree, id, options = options)
    },
    stop(ts_cnd(
      "Internal tsjsonc error, unknown JSON node type: '{tree$type[id]}'"
    ))
  )
}

which_line_comments <- function(tree, ids) {
  # this only works because `start_row` is sorted
  which(
    tree$type[ids] == "comment" &
      tree$end_row[ids - 1] == tree$start_row[ids]
  )
}

format_line_comments <- function(tree, elts, ids, format) {
  if (format == "pretty") {
    cmts <- which_line_comments(tree, ids)
    for (i in cmts) {
      # may happen if an array or object starts with a comment
      if (i == 1L) {
        next
      }
      elts[[i - 1]][length(elts[[i - 1]])] <- paste(
        elts[[i - 1]][length(elts[[i - 1]])],
        elts[[i]]
      )
      elts[i] <- list(NULL)
    }
  }
  elts
}

format_document <- function(tree, id, options) {
  stopifnot(tree$type[id] == "document")
  chdn <- tree$children[[id]]
  elts <- lapply(
    chdn,
    format_element,
    tree = tree,
    options = options
  )
  elts <- format_line_comments(tree, elts, chdn, options[["format"]])
  unlist(elts)
}

format_null <- function(tree, id, options) {
  stopifnot(tree$type[id] == "null")
  "null"
}

format_true <- function(tree, id, options) {
  stopifnot(tree$type[id] == "true")
  "true"
}

format_false <- function(tree, id, options) {
  stopifnot(tree$type[id] == "false")
  "false"
}

format_string <- function(tree, id, options) {
  stopifnot(tree$type[id] == "string")
  chdn <- tree$children[[id]]
  paste0(tree$code[chdn], collapse = "")
}

format_number <- function(tree, id, options) {
  stopifnot(tree$type[id] == "number")
  tree$code[id]
}

format_post_process_commas <- function(tree, elts, ids, format) {
  if (format != "pretty") {
    return(elts)
  }
  commas <- map_lgl(elts, function(x) {
    length(x) == 1 && startsWith(x, ",")
  })
  for (i in which(commas)) {
    if (tree$type[ids[i - 1]] == "comment") {
      next
    }
    elts[[i - 1]][length(elts[[i - 1]])] <- paste0(
      elts[[i - 1]][length(elts[[i - 1]])],
      elts[[i]]
    )
    elts[i] <- list(NULL)
  }
  elts
}

format_create_indent <- function(options) {
  if (options[["indent_style"]] == "space") {
    strrep(" ", options[["indent_width"]])
  } else {
    "\t"
  }
}

format_array <- function(tree, id, options) {
  stopifnot(tree$type[id] == "array")
  chdn <- tree$children[[id]]

  if (length(chdn) == 2) {
    return("[]")
  }

  chdn <- middle(chdn)
  elts <- lapply(
    chdn,
    format_element,
    tree = tree,
    options = options
  )

  elts <- format_line_comments(tree, elts, chdn, options[["format"]])
  elts <- format_post_process_commas(tree, elts, chdn, options[["format"]])

  indent <- format_create_indent(options)

  switch(
    options[["format"]],
    "compact" = {
      paste0("[", paste0(unlist(elts), collapse = ""), "]")
    },
    "oneline" = {
      paste0("[ ", paste0(unlist(elts), collapse = ""), " ]")
    },
    "pretty" = {
      c("[", paste0(indent, unlist(elts)), "]")
    }
  )
}

format_object <- function(tree, id, options) {
  stopifnot(tree$type[id] == "object")
  chdn <- tree$children[[id]]

  if (length(chdn) == 2) {
    return("{}")
  }

  chdn <- middle(chdn)
  elts <- lapply(
    chdn,
    format_element,
    tree = tree,
    options = options
  )
  elts <- format_line_comments(tree, elts, chdn, options[["format"]])
  elts <- format_post_process_commas(tree, elts, chdn, options[["format"]])

  indent <- format_create_indent(options)

  switch(
    options[["format"]],
    "compact" = {
      paste0("{", paste(unlist(elts), collapse = ""), "}")
    },
    "oneline" = {
      paste0("{ ", paste(unlist(elts), collapse = ""), " }")
    },
    "pretty" = {
      c(
        "{",
        paste0(indent, unlist(elts)),
        "}"
      )
    }
  )
}

# - Drop comments in compact and oneline mode.
# - Comments can only appear between the key and the value. Comments
#   before the key and after the value have the object as parent, not the
#   pair.
# - We put all comments _after_ the `:`, because we put the `:` on the
#   same line as the key.

format_pair <- function(tree, id, options) {
  stopifnot(tree$type[id] == "pair")
  chdn <- tree$children[[id]]
  key <- na_omit(chdn[tree$field_name[chdn] == "key"])
  keystr <- unserialize_string(tree, key)
  value <- na_omit(chdn[tree$field_name[chdn] == "value"])
  cmts <- chdn[tree$type[chdn] == "comment"]
  fvalue <- format_element(tree, value, options)

  indent <- format_create_indent(options)

  switch(
    options[["format"]],
    "compact" = {
      sprintf('"%s":%s', keystr, fvalue)
    },
    "oneline" = {
      sprintf('"%s": %s', keystr, fvalue)
    },
    "pretty" = {
      if (length(cmts) == 0) {
        fvalue[1] <- sprintf('"%s": %s', keystr, fvalue[1])
        fvalue
      } else {
        fcmts <- lapply(
          cmts,
          format_element,
          tree = tree,
          options = options
        )
        c(
          sprintf('"%s":', keystr),
          paste0(indent, unlist(fcmts)),
          paste0(indent, fvalue)
        )
      }
    }
  )
}

format_comment <- function(tree, id, options) {
  stopifnot(tree$type[id] == "comment")
  if (options[["format"]] == "pretty") {
    tree$code[id]
  } else {
    NULL
  }
}

format_comma <- function(tree, id, options) {
  stopifnot(tree$type[id] == ",")
  if (options[["format"]] == "oneline") {
    ", "
  } else {
    ","
  }
}
