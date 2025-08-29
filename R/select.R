#' @export

select <- function(json, ...) {
  select_(json, current = NULL, list(...))
}

#' @export

select_refine <- function(json, ...) {
  current <- attr(json, "selection")
  select_(json, current = current, list(...))
}

#' @export

select_add <- function(json, ...) {
  current <- attr(json, "selection")
  json <- select_(json, current = NULL, list(...))
  attr(json, "selection") <- sort(unique(c(current, attr(json, "selection"))))
  json
}

select_ <- function(json, current, slts) {
  if (is.null(current)) {
    itop <- json$children[[1L]]
    itop <- itop[json$type[itop] != "comment"]
    if (length(itop)) {
      current <- itop
    } else {
      current <- 1L
    }
  }

  for (slt in slts) {
    nxt <- integer()
    for (cur in current) {
      nxt <- c(nxt, select1(json, cur, slt))
    }
    current <- nxt
  }
  attr(json, "selection") <- unique(current)
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
    chdn <- chdn[!json$type[chdn] %in% c("[", ",", "]", "comment")]
    # TODO: select from the back
    if (Inf %in% slt) {
      na_omit(chdn)
    } else {
      na_omit(chdn[slt])
    }
    #
  } else {
    stop("Invalid JSON selector")
  }
}
