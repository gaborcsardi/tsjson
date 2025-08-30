# 1. there is an explicit selection
# 2. otherwise the top element is selected (or elements if many)
# 3. otherwise the document node is selected

get_selection <- function(json) {
  attr(json, "selection") %||% get_default_selection(json)
}

get_default_selection <- function(json) {
  top <- json$children[[1]]
  top <- top[json$type[top] != "comment"]
  if (length(top) > 0) {
    top
  } else {
    1L
  }
}

#' @export

select <- function(json, ...) {
  select_(json, current = NULL, list(...))
}

#' @export

select_refine <- function(json, ...) {
  current <- get_selection(json)
  select_(json, current = current, list(...))
}

#' @export

select_add <- function(json, ...) {
  # do not use get_selection() here, don't want to add to the whole document
  current <- attr(json, "selection")
  json <- select_(json, current = NULL, list(...))
  attr(json, "selection") <- sort(unique(c(current, attr(json, "selection"))))
  json
}

select_ <- function(json, current, slts) {
  current <- current %||% get_default_selection(json)

  for (slt in slts) {
    nxt <- integer()
    for (cur in current) {
      nxt <- c(nxt, select1(json, cur, slt))
    }
    current <- nxt
  }
  attr(json, "selection") <- sort(unique(current))
  json
}

select1 <- function(json, idx, slt) {
  type <- json$type[idx]
  row <- json$start_row[idx] + 1
  column <- json$start_column[idx] + 1
  if (is.character(slt)) {
    if (type != "object") {
      stop(cnd(
        "Cannot select named element of {type} at row {row}, column {column}."
      ))
    }
    pairs <- json$children[[idx]]
    pairs <- pairs[json$type[pairs] == "pair"]
    chdn <- unlist(json$children[pairs])
    keys <- chdn[!is.na(json$field_name[chdn]) & json$field_name[chdn] == "key"]
    keyvals <- map_chr(keys, unserialize_string, token_table = json)
    vals <- chdn[
      !is.na(json$field_name[chdn]) & json$field_name[chdn] == "value"
    ]
    vals[keyvals %in% slt]
  } else if (is.numeric(slt)) {
    chdn <- json$children[[idx]]
    chdn <- chdn[!json$type[chdn] %in% c("[", ",", "]", "{", "}", "comment")]
    # TODO: select from the back
    sel <- if (Inf %in% slt) {
      na_omit(chdn)
    } else {
      na_omit(chdn[slt])
    }
    # for objects we select the values
    if (type == "object") {
      sel <- map_int(sel, function(sel1) {
        gchdn <- json$children[[sel1]]
        gchdn <- gchdn[!is.na(json$field_name[gchdn])]
        gchdn[json$field_name[gchdn] == "value"]
      })
    }
    sel
  } else {
    stop("Invalid JSON selector")
  }
}
