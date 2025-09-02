#' @export

format_json <- function(
  file = NULL,
  text = NULL,
  format = c("pretty", "oneline", "compact")
) {
  format <- match.arg(format)

  # parse file/text
  # TODO: error on error, get error position
  json <- token_table(file = file, text = text)

  # it must have one non-comment element
  # multiple top-level values (e.g. JSONL) are not (yet) allowed
  top <- json$children[[1]]
  top <- top[json$type[top] != "comment"]
  stopifnot(length(top) == 1)

  format_element(json, top, format = format)
}

#' @export

format_selections <- function(
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

  deleted <- select
  while (TRUE) {
    deleted2 <- unique(c(deleted, unlist(json$children[deleted])))
    if (length(deleted2) == length(deleted)) {
      break
    }
    deleted <- deleted2
  }

  really_deleted <- setdiff(deleted, select)
  json$code[really_deleted] <- NA_character_
  json$tws[really_deleted] <- NA_character_
  json$code[select] <- map_chr(fmt, paste, collapse = "\n")

  parts <- c(rbind(json$code, json$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- load_json(text = text)
  attr(new, "file") <- attr(json, "file")

  new
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
    }
  )
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

format_array <- function(json, id, format) {
  stopifnot(json$type[id] == "array")
  chdn <- json$children[[id]]

  if (length(chdn) == 2) {
    return("[]")
  }

  chdn <- middle(chdn)
  elts <- lapply(chdn, format_element, json = json, format = format)

  switch(
    format,
    "compact" = {
      paste0("[", paste0(unlist(elts), collapse = ","), "]")
    },
    "oneline" = {
      paste0("[ ", paste0(unlist(elts), collapse = ", "), " ]")
    },
    "pretty" = {
      comm <- head(which(json$type[chdn] != "comment"), -1)
      for (i in comm) {
        elts[[i]][length(elts[[i]])] <- paste0(
          elts[[i]][length(elts[[i]])],
          ","
        )
      }
      c("[", paste0("  ", unlist(elts)), "]")
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
  felts <- lapply(chdn, format_element, json = json, format = format)

  switch(
    format,
    "compact" = {
      paste0("{", paste(unlist(felts), collapse = ","), "}")
    },
    "oneline" = {
      paste0("{ ", paste(unlist(felts), collapse = ", "), " }")
    },
    "pretty" = {
      needs_comma <- head(which(json$type[chdn] != "comment"), -1)
      for (i in needs_comma) {
        felts[[i]][length(felts[[i]])] <- paste0(
          felts[[i]][length(felts[[i]])],
          ","
        )
      }
      c(
        "{",
        paste0("  ", unlist(felts)),
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
          paste0("  ", unlist(fcmts)),
          paste0("  ", fvalue)
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
