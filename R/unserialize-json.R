#' @export

unserialize_json <- function(file = NULL, text = NULL) {
  # parse file/text
  # TODO: error on error, get error position
  tt <- token_table(file = file, text = text)

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

#' @export

unserialize_selections <- function(json) {
  sel <- get_selected_nodes(json)
  lapply(sel, unserialize_element, token_table = json)
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
