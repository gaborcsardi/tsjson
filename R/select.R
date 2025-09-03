# A section is a list of records. Each record has a selector
# and a list of selected nodes.
#
# 1. there is an explicit selection
# 2. otherwise the top element is selected (or elements if many)
# 3. otherwise the document node is selected

get_selection <- function(json, default = TRUE) {
  sel <- attr(json, "selection")
  if (!is.null(sel)) {
    sel
  } else if (default) {
    get_default_selection(json)
  } else {
    NULL
  }
}

get_selected_nodes <- function(json, default = TRUE) {
  sel <- get_selection(json, default = default)
  if (is.null(sel)) {
    return(integer())
  } else {
    sel[[length(sel)]]$nodes
  }
}

get_default_selection <- function(json) {
  top <- json$children[[1]]
  top <- top[json$type[top] != "comment"]
  list(
    list(
      selector = sel_default(),
      nodes = if (length(top) > 0) top else 1L
    )
  )
}

#' @export

select <- function(json, ...) {
  select_(json, current = NULL, list(...))
}

#' @export

deselect <- function(json) {
  attr(json, "selection") <- NULL
  json
}

#' @export

`[[.tsjson` <- function(x, i, ...) {
  if (missing(i)) {
    i <- list()
  }
  unserialize_selected(select(x, i, ...))
}

#' @export

`select<-` <- function(x, ..., value) {
  res <- if (inherits(value, "tsjson")) {
    value
  } else if (inherits(value, "tsjson_action_delete")) {
    delete_selected(select_refine(x, ...))
  } else {
    update_selected(select_refine(x, ...), value)
  }
  attr(res, "selection") <- NULL
  res
}

#' @export

`[[<-.tsjson` <- function(x, i, value) {
  if (missing(i)) {
    i <- list()
  }
  res <- if (inherits(value, "tsjson")) {
    value
  } else if (inherits(value, "tsjson_action_delete")) {
    delete_selected(select_refine(x, i))
  } else {
    update_selected(select_refine(x, i), value)
  }
  attr(res, "selection") <- NULL
  res
}

#' @export

select_refine <- function(json, ...) {
  current <- get_selection(json)
  select_(json, current = current, list(...))
}

select_ <- function(json, current, slts) {
  slts <- unlist(
    lapply(slts, function(x) {
      if (inherits(x, "tsjson_selector") || !is.list(x)) list(x) else x
    }),
    recursive = FALSE
  )
  current <- current %||% get_default_selection(json)
  cnodes <- current[[length(current)]]$nodes

  for (slt in slts) {
    nxt <- integer()
    for (cur in cnodes) {
      nxt <- unique(c(nxt, select1(json, cur, slt)))
    }
    current[[length(current) + 1L]] <- list(
      selector = slt,
      nodes = sort(nxt)
    )
    cnodes <- current[[length(current)]]$nodes
  }
  # if 'document' is selected, that means there is no selection
  if (identical(current[[1]]$nodes, 1L)) {
    attr(json, "selection") <- NULL
  } else {
    attr(json, "selection") <- current
  }
  json
}

select1 <- function(json, idx, slt) {
  type <- json$type[idx]
  sel <- if (inherits(slt, "tsjson_selector_all")) {
    chdn <- json$children[[idx]]
    chdn[!json$type[chdn] %in% c("[", ",", "]", "{", "}", "comment")]
  } else if (inherits(slt, "tsjson_selector_regex")) {
    if (type != "object") {
      integer()
    } else {
      pairs <- json$children[[idx]]
      pairs <- pairs[json$type[pairs] == "pair"]
      chdn <- unlist(json$children[pairs])
      keys <- chdn[
        !is.na(json$field_name[chdn]) & json$field_name[chdn] == "key"
      ]
      keyvals <- map_chr(keys, unserialize_string, token_table = json)
      pairs[grep(
        slt$regex,
        keyvals,
        ignore.case = slt$ignore_case,
        invert = slt$invert
      )]
    }
  } else if (inherits(slt, "tsjson_selector_back")) {
    chdn <- json$children[[idx]]
    chdn <- chdn[!json$type[chdn] %in% c("[", ",", "]", "{", "}", "comment")]
    rev(rev(chdn)[slt$v])
  } else if (inherits(slt, "tsjson_selector_ids")) {
    # this is special, select exactly these elements
    return(slt$ids)
  } else if (is.character(slt)) {
    if (type != "object") {
      integer()
    } else {
      pairs <- json$children[[idx]]
      pairs <- pairs[json$type[pairs] == "pair"]
      chdn <- unlist(json$children[pairs])
      keys <- chdn[
        !is.na(json$field_name[chdn]) & json$field_name[chdn] == "key"
      ]
      keyvals <- map_chr(keys, unserialize_string, token_table = json)
      pairs[keyvals %in% slt]
    }
  } else if (is.numeric(slt)) {
    chdn <- json$children[[idx]]
    chdn <- chdn[!json$type[chdn] %in% c("[", ",", "]", "{", "}", "comment")]
    chdn[slt]
  } else {
    stop("Invalid JSON selector")
  }

  # for objects we select the values, instead of the pairs
  if (type == "object") {
    sel <- map_int(sel, function(sel1) {
      gchdn <- json$children[[sel1]]
      gchdn <- gchdn[!is.na(json$field_name[gchdn])]
      gchdn[json$field_name[gchdn] == "value"]
    })
  }

  sel
}

sel_default <- function() {
  structure(
    list(),
    class = c("tsjson_selector_default", "tsjson_selector", "list")
  )
}

sel_ids <- function(ids) {
  structure(
    list(ids = ids),
    class = c("tsjson_selector_ids", "tsjson_selector", "list")
  )
}

#' @export

sel_all <- function() {
  structure(list(), class = c("tsjson_selector_all", "tsjson_selector", "list"))
}

#' @export

sel_regex <- function(regex, ignore_case = FALSE, invert = FALSE) {
  structure(
    list(regex = regex, ignore_case = ignore_case, invert = invert),
    class = c("tsjson_selector_regex", "tsjson_selector", "list")
  )
}

#' @export

sel_back <- function(v) {
  structure(
    list(v = v),
    class = c("tsjson_selector_back", "tsjson_selector", "list")
  )
}

#' @export

deleted <- function() {
  structure(
    list(),
    class = c("tsjson_action_delete", "tsjson_action", "list")
  )
}
