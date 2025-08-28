#' @export

# If the selection is an array or object, insert 'new' at 'index'.
# 'at' can be zero (beginning), Inf (end), index to insert _after_ index,
# (into an array), or a key to insert _after_ that key (into an object).

# If no selection then insert into the root element or at top level if
# none

insert_at_selection <- function(json, new, at = 0) {
  sel <- attr(json, "selection")
  # if no selection then insert at root element
  if (is.null(sel)) {
    sel <- json$children[[1L]]
    sel <- sel[json$type[sel] != "comment"]
  }
  # if no root element then at top level, under 'document'
  if (length(sel) == 0) {
    return(json)
  }

  insertions <- lapply(sel, function(sel1) {
    type <- json$type[sel1]
    if (type == "document") {
      # TODO
    } else if (type == "array") {
      if (!is.numeric(at)) {
        stop(cnd(
          "Invalid `at` value for inserting JSON element into array. \\
           It must be an integer scalar or `Inf`."
        ))
      }
      code <- serialize_json(new, collapse = TRUE)
      chdn <- json$children[[sel1]]
      nchdn <- if (length(chdn) == 2) {
        0
      } else {
        (length(chdn) - 1L) / 2L
      }
      precomma <- postcomma <- NULL
      after <- if (at < 1) {
        postcomma <- if (nchdn >= 1) ","
        chdn[1]
      } else if (at >= nchdn) {
        precomma <- if (nchdn >= 1) ","
        chdn[length(chdn) - 1L]
      } else {
        precomma <- ","
        chdn[at * 2L]
      }
      list(
        after = after,
        code = paste0(precomma, code, postcomma)
      )
    } else if (type == "object") {
      code <- paste0(
        '"',
        names(new),
        '": ',
        serialize_json(unname(new), collapse = TRUE)
      )
      chdn <- json$children[[sel1]]
      nchdn <- if (length(chdn) == 2) {
        0
      } else {
        (length(chdn) - 1L) / 2L
      }
      precomma <- postcomma <- NULL
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
      after <- if (at < 1) {
        postcomma <- if (nchdn >= 1) ","
        chdn[1]
      } else if (at >= nchdn) {
        precomma <- if (nchdn >= 1) ","
        chdn[length(chdn) - 1L]
      } else {
        precomma <- ","
        chdn[at * 2L]
      }
      list(
        after = after,
        code = paste0(precomma, code, postcomma)
      )
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
