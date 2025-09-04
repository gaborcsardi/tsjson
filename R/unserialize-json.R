#' Unserialize a JSON file or string into an R object
#'
#' The purpose of this function is to convert a JSON file or string into
#' an R object reliably.
#'
#' See examples below on how the different JSON elements are mapped to
#' R objects.
#'
#' @inheritParams sexpr_json
#' @return R object.
#'
#' @export
#' @seealso [serialize_json()] for the opposite, [select()] and
#' [unserialize_selected()] to unserialize part(s) of a JSON document.
#' [load_json()] to load a JSON document and then manipulate it.
#' @examples
#' # null -> NULL
#' unserialize_json(text = "null")
#'
#' # true, false -> TRUE, FALSE
#' unserialize_json(text = "true")
#' unserialize_json(text = "false")
#'
#' # string -> character scalar
#' unserialize_json(text = "\"string with escapes: \\b \\ud020\"")
#'
#' # number -> double scalar
#' unserialize_json(text = "42.25")
#'
#' # array -> unnamed list
#' unserialize_json(text = "[1, 2, 3]")
#'
#' # object -> named list
#' unserialize_json(text = "{\"a\": 1, \"b\": 2 }")

unserialize_json <- function(file = NULL, text = NULL, ranges = NULL) {
  # parse file/text
  # TODO: error on error, get error position
  tt <- token_table(file = file, text = text, ranges = ranges)

  # document is the top element. easier to process without NA parents
  # TODO: do not fail for empty file, but what to return? NULL, maybe?
  tt$parent[1] <- 0L

  # it must have one non-comment element
  # multiple top-level values (e.g. JSONL) are not (yet) allowed
  top <- tt$children[[1]]
  top <- top[tt$type[top] != "comment"]
  stopifnot(length(top) == 1)

  unserialize_element(tt, top)
}

#' Unserialize selected elements from a tsjson object
#'
#' Uses [unserialize_json()] on the selected elements.
#'
#' If `json` does not have a selection, then all of it is unserialized.
#' If `json` has an empty selection, then an empty list is returned.
#'
#' @param json tsjson object.
#' @return List of R objects, each the unserialization of a selected element
#'   in tsjson.
#'
#' @export
#' @seealso [unserialize_json()] to unserialize a JSON document from a
#'   file or string. [serialize_json()] to create JSON from R objects.
#' @examples
#' json <- load_json(text = serialize_json(list(
#'   a = list(a1 = list(1,2,3), a2 = "string"),
#'   b = list(4, 5, 6),
#'   c = list(c1 = list("a", "b"))
#' )))
#' json |> select(c("b", "c")) |> unserialize_selected()

unserialize_selected <- function(json) {
  sel <- get_selected_nodes(json)
  as.list(lapply(sel, unserialize_element, token_table = json))
}

unserialize_element <- function(token_table, id) {
  switch(
    token_table$type[id],
    null = {
      unserialize_null(token_table, id)
    },
    true = {
      unserialize_true(token_table, id)
    },
    false = {
      unserialize_false(token_table, id)
    },
    string = {
      unserialize_string(token_table, id)
    },
    number = {
      unserialize_number(token_table, id)
    },
    array = {
      unserialize_array(token_table, id)
    },
    object = {
      unserialize_object(token_table, id)
    }
  )
}

unserialize_null <- function(token_table, id) {
  stopifnot(token_table$type[id] == "null")
  NULL
}

unserialize_true <- function(token_table, id) {
  stopifnot(token_table$type[id] == "true")
  TRUE
}

unserialize_false <- function(token_table, id) {
  stopifnot(token_table$type[id] == "false")
  FALSE
}

unserialize_string <- function(token_table, id) {
  stopifnot(token_table$type[id] == "string")
  # escapes are almost the same as for R, but R does not have \/
  chdn <- token_table$children[[id]]
  str <- paste0(token_table$code[chdn], collapse = "")
  str <- gsub("\\/", "/", str, fixed = TRUE)
  # TODO: is there anything simpler than eval(parse(.))?
  eval(parse(text = str, keep.source = FALSE))
}

unserialize_number <- function(token_table, id) {
  stopifnot(token_table$type[id] == "number")
  # single token
  as.numeric(token_table$code[id])
}

unserialize_array <- function(token_table, id) {
  stopifnot(token_table$type[id] == "array")
  chdn <- token_table$children[[id]]
  # drop [ and , and ], parse the rest
  chdn <- chdn[!token_table$type[chdn] %in% c("[", ",", "]", "comment")]
  arr <- vector("list", length(chdn))
  for (idx in seq_along(chdn)) {
    arr[[idx]] <- unserialize_element(token_table, chdn[idx])
  }
  arr
}

unserialize_object <- function(token_table, id) {
  stopifnot(token_table$type[id] == "object")
  # keep the pairs, parse their names and values
  chdn <- token_table$children[[id]]
  chdn <- chdn[token_table$type[chdn] == "pair"]
  arr <- vector("list", length(chdn))
  nms <- character(length(chdn))
  for (idx in seq_along(chdn)) {
    gchdn <- token_table$children[[chdn[idx]]]
    gchdn <- gchdn[!is.na(token_table$field_name[gchdn])]
    key <- gchdn[token_table$field_name[gchdn] == "key"]
    nms[idx] <- unserialize_string(token_table, key)
    value <- gchdn[token_table$field_name[gchdn] == "value"]
    arr[[idx]] <- unserialize_element(token_table, value)
  }
  structure(arr, names = nms)
}
