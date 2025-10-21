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
#' @inheritParams token_table
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
  options = NULL
) {
  if (!missing(options)) {
    check_named_arg(options)
  }
  options <- as_tsjson_options(options, auto_format = TRUE)
  select <- get_selected_nodes(json)

  if (length(select) == 0) {
    return(json)
  }

  insertions <- lapply(select, function(sel1) {
    if (options[["format"]] == "auto") {
      options[["format"]] <- auto_format(json, sel1)
    }
    type <- json$type[sel1]
    if (type == "document") {
      insert_into_document(json, new, options)
    } else if (type == "array") {
      insert_into_array(json, sel1, new, at, options)
    } else if (type == "object") {
      insert_into_object(json, sel1, new, key, at, options)
    } else {
      stop(cnd(
        "Cannot insert into a '{type}' JSON element. Can only insert \\
         into 'array' and 'object' elements and empty JSON documents."
      ))
    }
  })

  insertions <- insertions[order(map_int(insertions, "[[", "after"))]

  for (ins in insertions) {
    if (!isFALSE(ins$leading_comma)) {
      aft <- last_descendant(json, ins$leading_comma)
      json$tws[aft] <- paste0(",", json$tws[aft])
    }

    aft <- last_descendant(json, ins$after)
    firstchld <- json$children[[ins$select]][1]
    # mark first child for reformatting the whole array
    json$tws[firstchld] <- paste0(reformat_mark, json$tws[firstchld])
    json$tws[aft] <- paste0(
      json$tws[aft],
      ins$code,
      if (ins$trailing_comma) ",",
      if (ins$trailing_newline) "\n"
    )
  }

  parts <- c(rbind(json$code, json$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- load_json(text = text)
  attr(new, "file") <- attr(json, "file")

  # now reformat the new parts, or the newly non-empty arrays/objects
  fws <- grepl(reformat_mark, new$tws, fixed = TRUE)
  fcd <- new$type == "comment" & grepl(reformat_mark, new$code, fixed = TRUE)
  new$tws[fws] <- gsub(reformat_mark, "", new$tws[fws], fixed = TRUE)
  new$code[fcd] <- gsub(reformat_mark, "", new$code[fcd], fixed = TRUE)
  tofmt2 <- unique(new$parent[which(fws | fcd)])

  # auto format then each insertion might need a different format
  if (options[["format"]] == "auto") {
    for (tofmt1 in tofmt2) {
      options[["format"]] <- auto_format(new, tofmt1)
      new <- format_selected(
        select(new, sel_ids(tofmt1)),
        options = options
      )
    }
  } else {
    new <- select(new, sel_ids(tofmt2))
    new <- format_selected(new, options = options)
  }
  new
}

last_descendant <- function(json, node) {
  while (node != 1 && is.na(json$code[node])) {
    node <- tail(json$children[[node]], 1)
  }
  node
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

insert_into_document <- function(json, new, options) {
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
    code = paste0(nl, serialize_json(new, collapse = TRUE, options = options)),
    leading_comma = FALSE,
    trailing_comma = FALSE,
    trailing_newline = FALSE # TODO
  )
}

insert_into_array <- function(json, sel1, new, at, format) {
  if (!is.numeric(at)) {
    stop(cnd(
      "Invalid `at` value for inserting JSON element into array. \\
           It must be an integer scalar or `Inf`."
    ))
  }

  # this is complicated by comments inside the array
  # we need to build an index to map non-comment children to actual children
  # we might as well treat the [ ] and comma nodes the same way
  chdn <- json$children[[sel1]]
  isxtr <- json$type[chdn] %in% c("comment", "[", "]", ",")
  idx <- seq_along(chdn)[!isxtr]
  nchdn <- length(idx)

  after_comma <- after <- if (at < 1 || nchdn == 0) {
    1
  } else if (at >= nchdn) {
    idx[nchdn]
  } else {
    idx[at]
  }

  if (
    json$type[chdn[after + 1L]] == "," &&
      json$type[chdn[after + 2L]] == "comment" &&
      json$end_row[chdn[after + 1L]] == json$start_row[chdn[after + 2L]]
  ) {
    # skip comma + comment on the same line!
    after <- after + 2L
  } else if (
    json$type[chdn[after + 1L]] == "comment" &&
      json$end_row[chdn[after]] == json$start_row[chdn[after + 1L]]
  ) {
    # skip comment on the same line
    after <- after + 1L
  } else if (json$type[chdn[after + 1L]] == ",") {
    # skip comma w/o comment on the same line
    # keep non-line comment as non-line comment
    after <- after + 1L
  } else {
    # skip comments and potentially a comma
    while (json$type[chdn[after + 1L]] == "comment") {
      after <- after + 1L
    }
    if (json$type[chdn[after + 1L]] == ",") {
      after <- after + 1L
    }
  }

  # handle appending when there is a trailig comma
  chdnx <- chdn[json$type[chdn] != "comment"]
  has_trailing_comma <- json$type[rev(chdnx)][2] == ","
  add_leading_comma <- !has_trailing_comma && at >= nchdn && nchdn > 0
  add_trailing_comma <- (at < nchdn && nchdn > 0) ||
    (at >= nchdn && has_trailing_comma)

  list(
    select = sel1,
    after = chdn[after],
    code = serialize_json(
      new,
      collapse = TRUE,
      options = list(format = "compact")
    ),
    # need a leading comma if inserting at the end into non-empty array
    leading_comma = if (add_leading_comma) chdn[after_comma] else FALSE,
    # need a trailing comma everywhere except at the end or in an empty array
    trailing_comma = add_trailing_comma,
    # if the next is a comment, it must be on a new line, keep it there
    trailing_newline = json$type[chdn[after + 1L]] == "comment"
  )
}

insert_into_object <- function(json, sel1, new, key, at, format) {
  chdn <- json$children[[sel1]]
  isxtr <- json$type[chdn] != "pair"
  idx <- seq_along(chdn)[!isxtr]
  nchdn <- length(idx)

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

  after_comma <- after <- if (at < 1 || nchdn == 0) {
    1
  } else if (at >= nchdn) {
    idx[nchdn]
  } else {
    idx[at]
  }

  if (
    json$type[chdn[after + 1L]] == "," &&
      json$type[chdn[after + 2L]] == "comment" &&
      json$end_row[chdn[after + 1L]] == json$start_row[chdn[after + 2L]]
  ) {
    # skip comma + comment on the same line!
    after <- after + 2L
  } else if (
    json$type[chdn[after + 1L]] == "comment" &&
      json$end_row[chdn[after]] == json$start_row[chdn[after + 1L]]
  ) {
    # skip comment on the same line
    after <- after + 1L
  } else if (json$type[chdn[after + 1L]] == ",") {
    # skip comma w/o comment on the same line
    # keep non-line comment as non-line comment
    after <- after + 1L
  } else {
    # skip comments and potentially a comma
    while (json$type[chdn[after + 1L]] == "comment") {
      after <- after + 1L
    }
    if (json$type[chdn[after + 1L]] == ",") {
      after <- after + 1L
    }
  }

  code <- paste0(
    '"',
    key,
    '":',
    serialize_json(new, collapse = TRUE, options = list(format = "compact"))
  )

  # handle appending when there is a trailig comma
  chdnx <- chdn[json$type[chdn] != "comment"]
  has_trailing_comma <- json$type[rev(chdnx)][2] == ","
  add_leading_comma <- !has_trailing_comma && at >= nchdn && nchdn > 0
  add_trailing_comma <- (at < nchdn && nchdn > 0) ||
    (at >= nchdn && has_trailing_comma)

  list(
    select = sel1,
    after = chdn[after],
    code = paste0(code, collapse = "\n"),
    # need a leading comma if inserting at the end into non-empty array
    leading_comma = if (add_leading_comma) chdn[after_comma] else FALSE,
    # need a trailing comma everywhere except at the end or in an empty array
    trailing_comma = add_trailing_comma,
    trailing_newline = json$type[chdn[after + 1L]] == "comment"
  )
}
