# If the selection is an array or object, insert 'new' at 'index'.
# 'at' can be zero (beginning), Inf (end), index to insert _after_ index,
# (into an array), or a key to insert _after_ that key (into an object).

# If no selection then insert into the root element or at top level if
# none

#' Insert a new element into the selected ones in a tsjson object
#'
#' Insert a new element into each selected array or object.
#'
#' It is an error trying to insert into an element that is not an array and
#' not an object.
#'
#' @param json tsjson object
#' @param new New element to insert. Will be serialized with
#'   [serialize_json()].
#' @param key Key of the new element, when inserting into an object.
#' @param at What position to insert the new element at:
#'   - `0`: at the beginning,
#'   - `Inf`: at the end,
#'   - other numbers: after the specified element,
#'   - a character scalar, the key after which the new element is inserted,
#'     if that key exists, when inserting into an object. If this key does
#'     not exist, then the new element is inserted at the end of the object.
#' @param format Formatting of the `new` element, passed to
#'  [serialize_json()].
#' @return The modified tsjson object.
#'
#' @export
#' @examples
#' json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
#' json
#'
#' json |> select("b") |> insert_into_selected("foo", at = 1)

insert_into_selected <- function(
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
      format <- auto_format(json, sel1)
    }
    type <- json$type[sel1]
    if (type == "document") {
      insert_into_document(json, new, format)
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
    # reformat whole array
    lastchld <- json$children[[ins$select]][2]
    json$tws[lastchld] <- paste0(reformat_mark, json$tws[lastchld])
    json$tws[aft] <- paste0(
      if (ins$leading_comma) ",",
      json$tws[aft],
      ins$code,
      if (ins$trailing_comma) ","
    )
  }

  parts <- c(rbind(json$code, json$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- load_json(text = text)
  attr(new, "file") <- attr(json, "file")

  # now reformat the new parts, or the newly non-empty arrays/objects
  tofmt <- grep(reformat_mark, new$tws, fixed = TRUE)
  new$tws[tofmt] <- gsub(reformat_mark, "", new$tws[tofmt], fixed = TRUE)
  tofmt2 <- new$parent[tofmt]

  # auto format then each insertion might need a different format
  if (format == "auto") {
    for (tofmt1 in tofmt2) {
      format <- auto_format(new, tofmt1)
      new <- format_selected(select(new, sel_ids(tofmt1)), format = format)
    }
  } else {
    new <- select(new, sel_ids(tofmt2))
    new <- format_selected(new, format = format)
  }
  new
}

auto_format <- function(json, sel) {
  # if the array/object spans multiple lines then pretty formatting
  if (json$start_row[sel] != json$end_row[sel]) {
    "pretty"
  } else {
    # if there is no space after children (except the last ], }), compact
    # except if it is the empty array/object
    chdn <- json$children[[sel]]
    if (length(chdn) == 2 || sel == 1L) {
      "pretty"
    } else if (all(json$tws[head(chdn, -1)] == "")) {
      "compact"
    } else {
      "oneline"
    }
  }
}

reformat_mark <- "\f"

insert_into_document <- function(json, new, format) {
  top <- json$children[[1]]
  notcmt <- top[json$type[top] != "comment"]
  # TODO: can this ever happen?
  if (length(notcmt) != 0) {
    stop(cnd(
      "Cannot insert JSON element at the document root if the document \\
       already has other non-comment elements."
    ))
  }

  text <- attr(json, "text")
  nl <- if (length(text) > 0 && text[length(text)] != 0xa) {
    "\n"
  }

  list(
    select = 1L,
    after = nrow(json),
    code = paste0(nl, serialize_json(new, collapse = TRUE, format = format)),
    leading_comma = FALSE,
    trailing_comma = FALSE
  )
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
  after <- if (at < 1) {
    chdn[1]
  } else if (at >= nchdn) {
    chdn[length(chdn) - 1L]
  } else {
    chdn[at * 2L]
  }

  list(
    select = sel1,
    after = after,
    code = serialize_json(new, collapse = TRUE, format = "compact"),
    leading_comma = at < 1 || (at >= nchdn && nchdn >= 1) || at < nchdn,
    trailing_comma = at < 1 && nchdn >= 1
  )
}

insert_into_object <- function(json, sel1, new, key, at, format) {
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
  after <- if (at < 1) {
    chdn[1]
  } else if (at >= nchdn) {
    chdn[length(chdn) - 1L]
  } else {
    chdn[at * 2L]
  }

  code <- paste0(
    '"',
    key,
    '":',
    serialize_json(new, collapse = TRUE, format = "compact")
  )

  list(
    select = sel1,
    after = after,
    code = paste0(code, collapse = "\n"),
    leading_comma = at < 1 || (at >= nchdn && nchdn >= 1) || at < nchdn,
    trailing_comma = at < 1 && nchdn >= 1
  )
}
