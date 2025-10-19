format_json <- function(
  file = NULL,
  text = NULL,
  format = c("pretty", "oneline", "compact"),
  options = NULL
) {
  if (!missing(options)) {
    check_named_arg(options)
  }
  options <- as_tsjson_options(options)
  format <- match.arg(format)

  # parse file/text
  # TODO: error on error, get error position
  json <- token_table(file = file, text = text, options = options)
  format_element(json, 1L, format = format)
}

#' Format the selected JSON elements
#'
#' @details
#' If `json` does not have a selection, then all of it is formatted.
#' If `json` has an empty selection, then nothing happens.
#'
#' @param json tsjson object.
#' @param format Formatting, one of:
#'   - `"pretty"`: arrays and objects are formatted in multiple lines,
#'   - `"compact"`: format everything without whitespace,
#'   - `"oneline"`: format everything without newlines, but include
#'     whitespace after commas, colons, opening brackets and braces, and
#'     before closing brackets and braces.
#' @return The updated tsjson object.
#'
#' @export
#' @examples
#' json <- load_json(text = "{ \"a\": [1,2,3] }")
#' json
#'
#' json |> format_selected()
#'
#' json |> select("a") |> format_selected()

format_selected <- function(
  json,
  format = c("pretty", "oneline", "compact")
) {
  format <- match.arg(format)
  select <- get_selected_nodes(json)
  fmt <- lapply(select, format_element, json = json, format = format)
  for (i in seq_along(select)) {
    sel1 <- select[i]
    prevline <- rev(which(json$end_row == json$start_row[sel1] - 1))[1]
    ind0 <- sub("^.*\n", "", json$tws[prevline])
    if (!is.na(prevline)) {
      fmt[[i]] <- paste0(c("", rep(ind0, length(fmt[[i]]) - 1L)), fmt[[i]])
    }
  }

  subtrees <- lapply(select, get_subtree, json = json, with_root = FALSE)
  deleted <- unique(unlist(subtrees))

  # need to keep the trailing ws of the last element
  lasts <- map_int(subtrees, max_or_na)
  tws <- json$tws[lasts]
  json$code[deleted] <- NA_character_
  json$tws[deleted] <- NA_character_
  json$code[select] <- paste0(
    map_chr(fmt, paste, collapse = "\n"),
    ifelse(is.na(tws), "", tws)
  )
  json$tws[select] <- NA_character_

  parts <- c(rbind(json$code, json$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- load_json(text = text)
  attr(new, "file") <- attr(json, "file")

  new
}

get_subtree <- function(json, id, with_root = FALSE) {
  sel <- c(if (with_root) id, json$children[[id]])
  while (TRUE) {
    sel2 <- unique(c(sel, unlist(json$children[sel])))
    if (length(sel2) == length(sel)) {
      return(sel)
    }
    sel <- sel2
  }
}

format_element <- function(json, id, format) {
  switch(
    json$type[id],
    null = {
      format_null(json, id, format = format)
    },
    true = {
      format_true(json, id, format = format)
    },
    false = {
      format_false(json, id, format = format)
    },
    string = {
      format_string(json, id, format = format)
    },
    number = {
      format_number(json, id, format = format)
    },
    array = {
      format_array(json, id, format = format)
    },
    object = {
      format_object(json, id, format = format)
    },
    comment = {
      format_comment(json, id, format = format)
    },
    pair = {
      format_pair(json, id, format = format)
    },
    document = {
      format_document(json, id, format = format)
    },
    "," = {
      format_comma(json, id, format = format)
    },
    stop(cnd(
      "Internal tsjson error, unknown JSON node type: '{json$type[id]}'"
    ))
  )
}

which_line_comments <- function(json, ids) {
  # this only works because `start_row` is sorted
  which(
    json$type[ids] == "comment" &
      json$end_row[ids - 1] == json$start_row[ids]
  )
}

format_line_comments <- function(json, elts, ids, format) {
  if (format == "pretty") {
    cmts <- which_line_comments(json, ids)
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

format_document <- function(json, id, format) {
  stopifnot(json$type[id] == "document")
  chdn <- json$children[[id]]
  elts <- lapply(chdn, format_element, json = json, format = format)
  elts <- format_line_comments(json, elts, chdn, format)
  unlist(elts)
}

format_null <- function(json, id, format) {
  stopifnot(json$type[id] == "null")
  "null"
}

format_true <- function(json, id, format) {
  stopifnot(json$type[id] == "true")
  "true"
}

format_false <- function(json, id, format) {
  stopifnot(json$type[id] == "false")
  "false"
}

format_string <- function(json, id, format) {
  stopifnot(json$type[id] == "string")
  chdn <- json$children[[id]]
  paste0(json$code[chdn], collapse = "")
}

format_number <- function(json, id, format) {
  stopifnot(json$type[id] == "number")
  json$code[id]
}

format_indent <- 4

format_post_process_commas <- function(elts, format) {
  if (format != "pretty") {
    return(elts)
  }
  commas <- map_lgl(elts, function(x) {
    length(x) == 1 && startsWith(x, ",")
  })
  for (i in which(commas)) {
    elts[[i - 1]][length(elts[[i - 1]])] <- paste0(
      elts[[i - 1]][length(elts[[i - 1]])],
      elts[[i]]
    )
    elts[i] <- list(NULL)
  }
  elts
}

format_array <- function(json, id, format) {
  stopifnot(json$type[id] == "array")
  chdn <- json$children[[id]]

  if (length(chdn) == 2) {
    return("[]")
  }

  chdn <- middle(chdn)
  elts <- lapply(chdn, format_element, json = json, format = format)
  elts <- format_line_comments(json, elts, chdn, format)
  elts <- format_post_process_commas(elts, format)

  indent <- strrep(" ", format_indent)

  switch(
    format,
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

format_object <- function(json, id, format) {
  stopifnot(json$type[id] == "object")
  chdn <- json$children[[id]]

  if (length(chdn) == 2) {
    return("{}")
  }

  chdn <- middle(chdn)
  elts <- lapply(chdn, format_element, json = json, format = format)
  elts <- format_line_comments(json, elts, chdn, format)
  elts <- format_post_process_commas(elts, format)

  indent <- strrep(" ", format_indent)

  switch(
    format,
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

format_pair <- function(json, id, format) {
  stopifnot(json$type[id] == "pair")
  chdn <- json$children[[id]]
  key <- na_omit(chdn[json$field_name[chdn] == "key"])
  keystr <- unserialize_string(json, key)
  value <- na_omit(chdn[json$field_name[chdn] == "value"])
  cmts <- chdn[json$type[chdn] == "comment"]
  fvalue <- format_element(json, value, format)

  indent <- strrep(" ", format_indent)

  switch(
    format,
    "compact" = {
      glue('"{keystr}":{fvalue}')
    },
    "oneline" = {
      glue('"{keystr}": {fvalue}')
    },
    "pretty" = {
      if (length(cmts) == 0) {
        fvalue[1] <- glue('"{keystr}": {fvalue[1]}')
        fvalue
      } else {
        fcmts <- lapply(cmts, format_element, json = json, format = format)
        c(
          glue('"{keystr}":'),
          paste0(indent, unlist(fcmts)),
          paste0(indent, fvalue)
        )
      }
    }
  )
}

format_comment <- function(json, id, format) {
  stopifnot(json$type[id] == "comment")
  if (format == "pretty") {
    json$code[id]
  } else {
    NULL
  }
}

format_comma <- function(json, id, format) {
  stopifnot(json$type[id] == ",")
  if (format == "oneline") {
    ", "
  } else {
    ","
  }
}
