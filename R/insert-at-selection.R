#' @export

# If the selection is an array or object, insert 'new' at 'index'.
# 'at' can be zero (beginning), Inf (end), index to insert _after_ index,
# (into an array), or a key to insert _after_ that key (into an object).

# If no selection then insert into the root element or at top level if
# none

insert_at_selections <- function(
  json,
  new,
  key = NULL,
  at = Inf,
  format = c("auto", "pretty", "compact", "oneline")
) {
  format <- match.arg(format)
  select <- get_selected_nodes(json)

  if (length(select) == 0) {
    return(json)
  }

  insertions <- lapply(select, function(sel1) {
    if (format == "auto") {
      # if the array/object spans multiple lines then pretty formatting
      format <- if (json$start_row[sel1] != json$end_row[sel1]) {
        "pretty"
      } else {
        # if there is no space after children (except the last ], }), compact
        # except if it is the empty array/object
        chdn <- json$children[[sel1]]
        if (length(chdn) == 2) {
          "pretty"
        } else if (all(json$tws[head(chdn, -1)] == "")) {
          "compact"
        } else {
          "oneline"
        }
      }
    }
    type <- json$type[sel1]
    if (type == "document") {
      stop(cnd("Inserting into a document element is not implemented yet"))
    } else if (type == "array") {
      insert_into_array(json, sel1, new, at, format)
    } else if (type == "object") {
      insert_into_object(json, sel1, new, key, at, format)
    }
  })

  insertions <- insertions[order(map_int(insertions, "[[", "after"))]

  for (ins in insertions) {
    aft <- ins$after
    # need to add the new text after the last terminal node of aft,
    # except if it is the document node
    while (aft != 1 && is.na(json$code[aft])) {
      aft <- tail(json$children[[aft]], 1)
    }
    json$tws[aft] <- paste0(json$tws[aft], ins$code)
  }

  parts <- c(rbind(json$code, json$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- load_json(text = text)
  attr(new, "file") <- attr(json, "file")

  new
}

insert_into_array <- function(json, sel1, new, at, format) {
  if (!is.numeric(at)) {
    stop(cnd(
      "Invalid `at` value for inserting JSON element into array. \\
           It must be an integer scalar or `Inf`."
    ))
  }
  chdn <- json$children[[sel1]]
  nchdn <- if (length(chdn) == 2) {
    0
  } else {
    (length(chdn) - 1L) / 2L
  }
  cc <- if (format == "oneline") ", " else ","
  lc <- tc <- NULL
  after <- if (at < 1) {
    tc <- if (nchdn >= 1) cc
    chdn[1]
  } else if (at >= nchdn) {
    lc <- if (nchdn >= 1) cc
    chdn[length(chdn) - 1L]
  } else {
    lc <- cc
    chdn[at * 2L]
  }
  # need the indentation of the array element itself
  # take the last trailing whitespace of the previous line
  code <- serialize_json(new, collapse = FALSE, format = format)
  ind <- if (format == "pretty") {
    prevline <- rev(which(json$end_row == json$start_row[sel1] - 1))[1]
    ind0 <- sub("^.*\n", "", json$tws[prevline])
    if (is.na(prevline)) {
      ""
    } else {
      c("  ", rep(paste0(ind0, "  "), length(code) - 1L))
    }
  }
  lws <- switch(
    format,
    "pretty" = if (is.null(lc)) paste0("\n", ind0),
    "oneline" = " ",
    ""
  )
  tws <- switch(format, "pretty" = paste0("\n", ind0), "oneline" = " ", "")
  list(
    after = after,
    code = paste0(lc, lws, paste0(ind, code, collapse = "\n"), tc, tws)
  )
}

insert_into_object <- function(json, sel1, new, key, at, format) {
  code <- serialize_json(new, collapse = FALSE, format = format)
  code[1] <- paste0('"', key, switch(format, compact = '":', '": '), code[1])
  chdn <- json$children[[sel1]]
  nchdn <- if (length(chdn) == 2) {
    0
  } else {
    (length(chdn) - 1L) / 2L
  }
  if (is.character(at)) {
    rchdn <- chdn[json$type[chdn] == "pair"]
    keys <- map_chr(rchdn, function(id) {
      gchdn <- json$children[[id]]
      gchdn <- gchdn[!is.na(json$field_name[gchdn])]
      keyid <- gchdn[json$field_name[gchdn] == "key"]
      unserialize_string(json, keyid)
    })
    at <- match(at, keys)
    if (is.na(at)) {
      at <- Inf
    }
  }
  cc <- if (format == "oneline") ", " else ","
  lc <- tc <- NULL
  after <- if (at < 1) {
    tc <- if (nchdn >= 1) cc
    chdn[1]
  } else if (at >= nchdn) {
    lc <- if (nchdn >= 1) cc
    chdn[length(chdn) - 1L]
  } else {
    lc <- cc
    chdn[at * 2L]
  }
  # need the indentation of the array element itself
  # take the last trailing whitespace of the previous line
  ind <- if (format == "pretty") {
    prevline <- rev(which(json$end_row == json$start_row[sel1] - 1))[1]
    ind0 <- sub("^.*\n", "", json$tws[prevline])
    if (is.na(prevline)) {
      ""
    } else {
      c("  ", rep(paste0(ind0, "  "), length(code) - 1L))
    }
  }
  lws <- switch(
    format,
    "pretty" = paste0("\n", ind0),
    "oneline" = " ",
    ""
  )
  tws <- switch(format, "pretty" = paste0("\n", ind0), "oneline" = " ", "")
  list(
    after = after,
    code = paste0(lc, lws, paste0(ind, code, collapse = "\n"), tc, tws)
  )
}
