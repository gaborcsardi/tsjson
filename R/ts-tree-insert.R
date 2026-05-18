# If the selection is an array or object, insert 'new' at 'index'.
# 'at' can be zero (beginning), Inf (end), index to insert _after_ index,
# (into an array), or a key to insert _after_ that key (into an object).

# If no selection then insert into the root element or at top level if
# none

#' Insert a new element into the selected ones in a ts_tree_jsonc object
#'
#' Insert a new element into each selected array or object.
#'
#' It is an error trying to insert into an element that is not an array and
#' not an object.
#'
#' @ts ts_tree_insert_details_errors
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' json |> ts_tree_select("a") |> ts_tree_insert("foo")
#' ```
#'
#' @ts ts_tree_insert_details_no_selection
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' json |> ts_tree_insert(key = "c", new = "foo")
#' ```
#'
#' @ts ts_tree_insert_details_empty_selection
#'
#' ```{asciicast}
#' json <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' json |> ts_tree_select("nonexistent") |> ts_tree_insert("foo")
#' ```
#'
#' @param tree ts_tree_jsonc object
#' @param new New element to insert. Will be serialized with
#'   [ts_serialize_jsonc()].
#' @param key Key of the new element, when inserting into an object.
#' @param at What position to insert the new element at:
#'   - `0`: at the beginning,
#'   - `Inf`: at the end,
#'   - other numbers: after the specified element,
#'   - a character scalar, the key after which the new element is inserted,
#'     if that key exists, when inserting into an object. If this key does
#'     not exist, then the new element is inserted at the end of the object.
#' @param options Named list of formatting options, see
#'   [tsjsonc options][tsjsonc_options].
#' @param ... Must be empty currently, reserved for future use.
#' @return The modified ts_tree_jsonc object.
#'
#' @export
#' @keywords internal
#'
#' @examples
#' library(tsitter)
#' json <- ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' json
#'
#' json |> ts_tree_select("b") |> ts_tree_insert("foo", at = 1)

ts_tree_insert.ts_tree_jsonc <- function(
  tree,
  new,
  key = NULL,
  at = Inf,
  options = NULL,
  ...
) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tsjsonc_options(options, auto_format = TRUE)
  select <- ts_tree_selected_nodes(tree)
  # TODO: check that ... is empty

  if (length(select) == 0) {
    return(tree)
  }

  insertions <- lapply(select, function(sel1) {
    if (options[["format"]] == "auto") {
      options[["format"]] <- auto_format(tree, sel1)
    }
    type <- tree$type[sel1]
    if (type == "document") {
      insert_into_document(tree, new, options)
    } else if (type == "array") {
      insert_into_array(tree, sel1, new, at, options)
    } else if (type == "object") {
      insert_into_object(tree, sel1, new, key, at, options)
    } else {
      stop(ts_cnd(
        "Cannot insert into a '{type}' JSON element. Can only insert \\
         into 'array' and 'object' elements and empty JSON documents."
      ))
    }
  })

  insertions <- insertions[order(map_int(insertions, "[[", "after"))]

  for (ins in insertions) {
    if (!isFALSE(ins$leading_comma)) {
      aft <- last_descendant(tree, ins$leading_comma)
      tree$tws[aft] <- paste0(",", tree$tws[aft])
    }

    aft <- last_descendant(tree, ins$after)
    firstchld <- tree$children[[ins$select]][1]
    # mark first child for reformatting the whole array
    tree$tws[firstchld] <- paste0(reformat_mark, tree$tws[firstchld])
    tree$tws[aft] <- paste0(
      tree$tws[aft],
      ins$code,
      if (ins$trailing_comma) ",",
      if (ins$trailing_newline) "\n"
    )
  }

  parts <- c(rbind(tree$code, tree$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- ts_parse_jsonc(text = text)
  attr(new, "file") <- attr(tree, "file")

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
      new <- ts_tree_format(
        ts_tree_select(new, I(tofmt1)),
        options = options
      )
    }
  } else {
    new <- ts_tree_select(new, I(tofmt2))
    new <- ts_tree_format(new, options = options)
  }
  new
}

last_descendant <- function(tree, node) {
  while (node != 1 && is.na(tree$code[node])) {
    node <- tail(tree$children[[node]], 1)
  }
  node
}

auto_format <- function(tree, sel) {
  # if the array/object spans multiple lines then pretty formatting
  if (tree$start_row[sel] != tree$end_row[sel]) {
    "pretty"
  } else {
    # if there is no space after children (except the last ], }), compact
    # except if it is the empty array/object
    chdn <- tree$children[[sel]]
    if (length(chdn) == 2 || sel == 1L) {
      "pretty"
    } else if (all(tree$tws[head(chdn, -1)] == "")) {
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
    stop(ts_cnd(
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
    code = paste0(
      nl,
      ts_serialize_jsonc(new, collapse = TRUE, options = options)
    ),
    leading_comma = FALSE,
    trailing_comma = FALSE,
    trailing_newline = FALSE # TODO
  )
}

insert_into_array <- function(json, sel1, new, at, format) {
  if (!is.numeric(at)) {
    stop(ts_cnd(
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
    code = ts_serialize_jsonc(
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
    ts_serialize_jsonc(new, collapse = TRUE, options = list(format = "compact"))
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
