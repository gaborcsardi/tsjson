#' Unserialize selected elements from a ts_tree_jsonc object
#'
#' Uses [ts_tree_unserialize()] on the selected elements.
#'
#' If `json` does not have a selection, then all of it is unserialized.
#' If `json` has an empty selection, then an empty list is returned.
#'
#' @ts ts_tree_unserialize_details_no_selection
#'
#' ```{asciicast}
#' #| results = "hide"
#' tree <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' ```
#'
#' ```{asciicast}
#' ts_tree_unserialize(tree)
#' ```
#'
#' @ts ts_tree_unserialize_details_empty_selection
#'
#' ```{asciicast}
#' #| results = "hide"
#' tree <- tsjsonc::ts_parse_jsonc("{ \"a\": true, \"b\": [1, 2, 3] }")
#' ```
#'
#' ```{asciicast}
#' tree |> ts_tree_select("nope") |> ts_tree_unserialize()
#' ```
#'
#' @param tree ts_tree_jsonc object.
#' @return List of R objects, each the unserialization of a selected element
#'   in tsjsonc.
#'
#' @export
#' @keywords internal
#'
#' @seealso [ts_tree_unserialize()] to unserialize a JSON document from a
#'   file or string. [ts_serialize_jsonc()] to create JSON from R objects.
#' @examples
#' library(tsitter)
#' json <- ts_parse_jsonc(ts_serialize_jsonc(list(
#'   a = list(a1 = list(1,2,3), a2 = "string"),
#'   b = list(4, 5, 6),
#'   c = list(c1 = list("a", "b"))
#' )))
#' json |> ts_tree_select(c("b", "c")) |> ts_tree_unserialize()

ts_tree_unserialize.ts_tree_jsonc <- function(tree) {
  sel <- ts_tree_selected_nodes(tree)
  as.list(lapply(sel, unserialize_element, tree = tree))
}

unserialize_element <- function(tree, id) {
  switch(
    tree$type[id],
    document = {
      # this only happens for empty documents, otherwise we select the
      # a root element that is below document
      NULL
    },
    null = {
      unserialize_null(tree, id)
    },
    true = {
      unserialize_true(tree, id)
    },
    false = {
      unserialize_false(tree, id)
    },
    string = {
      unserialize_string(tree, id)
    },
    number = {
      unserialize_number(tree, id)
    },
    array = {
      unserialize_array(tree, id)
    },
    object = {
      unserialize_object(tree, id)
    }
  )
}

unserialize_null <- function(tree, id) {
  stopifnot(tree$type[id] == "null")
  NULL
}

unserialize_true <- function(tree, id) {
  stopifnot(tree$type[id] == "true")
  TRUE
}

unserialize_false <- function(tree, id) {
  stopifnot(tree$type[id] == "false")
  FALSE
}

unserialize_string <- function(tree, id) {
  stopifnot(tree$type[id] == "string")
  # escapes are almost the same as for R, but R does not have \/
  chdn <- tree$children[[id]]
  str <- paste0(tree$code[chdn], collapse = "")
  str <- gsub("\\/", "/", str, fixed = TRUE)
  # TODO: is there anything simpler than eval(parse(.))?
  eval(parse(text = str, keep.source = FALSE))
}

unserialize_number <- function(tree, id) {
  stopifnot(tree$type[id] == "number")
  # single token
  num <- as.numeric(tree$code[id])
  # integer or double
  numi <- suppressWarnings(as.integer(num))
  if (num == numi) {
    numi
  } else {
    num
  }
}

unserialize_array <- function(tree, id) {
  stopifnot(tree$type[id] == "array")
  chdn <- tree$children[[id]]
  # drop [ and , and ], parse the rest
  chdn <- chdn[!tree$type[chdn] %in% c("[", ",", "]", "comment")]
  arr <- vector("list", length(chdn))
  for (idx in seq_along(chdn)) {
    arr[idx] <- list(unserialize_element(tree, chdn[idx]))
  }
  arr
}

unserialize_object <- function(tree, id) {
  stopifnot(tree$type[id] == "object")
  # keep the pairs, parse their names and values
  chdn <- tree$children[[id]]
  chdn <- chdn[tree$type[chdn] == "pair"]
  arr <- vector("list", length(chdn))
  nms <- character(length(chdn))
  for (idx in seq_along(chdn)) {
    gchdn <- tree$children[[chdn[idx]]]
    gchdn <- gchdn[!is.na(tree$field_name[gchdn])]
    key <- gchdn[tree$field_name[gchdn] == "key"]
    nms[idx] <- unserialize_string(tree, key)
    value <- gchdn[tree$field_name[gchdn] == "value"]
    arr[idx] <- list(unserialize_element(tree, value))
  }
  structure(arr, names = nms)
}
