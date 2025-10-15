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

#' Select elements in a tsjson object
#'
#' This function is the heart of tsjson. To delete or manipulate parts of
#' a JSON document, you need to [select()] those parts first. To insert new
#' elements into a JSON document, you need to select the arrays or objects
#' the elements will be inserted into.
#'
#' ## Selectors
#'
#' You can use a list of selectors to iteratively refine the selection
#' of JSON elements, starting from the document element (the default
#' selection).
#'
#' For [select()] the list of selectors may be specified in a single list
#' argument, or as multiple arguments.
#'
#' Available selectors:
#' - `TRUE` selects all child elements of the current selections.
#' - A character vector selects the named child elements from selected
#'   objects. Selects nothing from arrays.
#' - A numeric vector selectes the listed child elements from selected
#'   arrays or objects. Positive (1-based) indices are counted from the
#'   beginning, negative indices are counted from the end of the array or
#'   object. E.g. -1 is the last element (if any).
#' - A character scalar named `"regex"`, with a regular expression.
#'   It selects the child elements whose keys match the regular expression.
#'   Selects nothing from arrays.
#'
#' ## Refining selections
#'
#' [select_refine()] is similar to [select()], but it starts from the
#' already selected elements (all of them simultanously), instead of
#' starting from the document element.
#'
#' ## The `[[` and `[[<-` operators
#'
#' The `[[` operator works similarly to [select_refine()] on tsjson objects,
#' but it might be more readable.
#'
#' The `[[<-` operator works similarly to [select<-()], but it might be
#' more readable.
#'
#' @param x,json tsjson object.
#' @param i,... Selectors, see below.
#' @return A tsjson object, potentially with some elements selected.
#'
#' @export
#' @examples
#' json <- load_json(text = serialize_json(list(
#'   a = list(a1 = list(1,2,3), a2 = "string"),
#'   b = list(4, 5, 6),
#'   c = list(c1 = list("a", "b"))
#' )))
#'
#' json
#'
#' # Select object by key
#' json |> select("a")
#'
#' # Select within select, these are the same
#' json |> select("a", "a1")
#' json |> select(list("a", "a1"))
#'
#' # Select elements of an array
#' json |> select("b", TRUE)           # all elements
#' json |> select("b", 1:2)            # first two elements
#' json |> select("b", c(1, -1))       # first and last elements
#'
#' # Regular expressions
#' json |> select(c("a", "c"), c(regex = "1$"))

select <- function(json, ...) {
  slts <- list(...)
  if (length(slts) == 1 && is.null(slts[[1]])) {
    attr(json, "selection") <- NULL
    json
  } else {
    select_(json, current = NULL, slts)
  }
}

#' @rdname select
#' @export

`[[.tsjson` <- function(x, i, ...) {
  if (missing(i)) {
    i <- list()
  }
  unserialize_selected(select(x, i, ...))
}

#' Update selected elements in a tsjson object
#'
#' Update the selected elements of a JSON document, using the replacement
#' function syntax.
#'
#' Technically [select<-()] is equivalent to [select_refine()] plus
#' [update_selected()]. In case when `value` is
#'
#' @param x,json tsjson object. Create a tsjson object with [load_json()].
#' @param i,... Selectors, see [select()].
#' @param value New value. Will be serialized to JSON with [serialize_json()].
#' @return The updated tsjson object.
#'
#' @seealso Save the updated tjson object to a file with [save_json()].
#'
#' @name select-set
#' @export

`select<-` <- function(json, ..., value) {
  res <- if (inherits(value, "tsjson")) {
    value # nocov
  } else if (inherits(value, "tsjson_action_delete")) {
    delete_selected(select_refine(json, ...))
  } else {
    update_selected(select_refine(json, ...), value)
  }
  attr(res, "selection") <- NULL
  res
}

#' @rdname select-set
#' @export

`[[<-.tsjson` <- function(x, i, value) {
  if (missing(i)) {
    i <- list()
  }
  res <- if (inherits(value, "tsjson")) {
    value # nocov
  } else if (inherits(value, "tsjson_action_delete")) {
    delete_selected(select_refine(x, i))
  } else {
    update_selected(select_refine(x, i), value)
  }
  attr(res, "selection") <- NULL
  res
}

#' @rdname select
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
  sel <- if (identical(slt, TRUE)) {
    chdn <- json$children[[idx]]
    chdn[!json$type[chdn] %in% c("[", ",", "]", "{", "}", "comment")]
  } else if (is.character(slt) && identical(names(slt), "regex")) {
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
      pairs[grep(slt, keyvals)]
    }
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
    if (any(slt == 0)) {
      stop(cnd("Zero indices are not allowed in JSON selectors."))
    }
    chdn <- json$children[[idx]]
    chdn <- chdn[!json$type[chdn] %in% c("[", ",", "]", "{", "}", "comment")]
    res <- integer(length(slt))
    pos <- slt >= 0
    if (any(pos)) {
      res[pos] <- chdn[slt[pos]]
    }
    if (any(!pos)) {
      res[!pos] <- rev(rev(chdn)[abs(slt[!pos])])
    }
    res
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

#' @rdname select-set
#' @usage NULL
#' @details
#' [deleted()] is a special marker to delete elements from a tsjson object
#' with [select<-()] or the double bracket operator.
#'
#' @return [deleted()] returns a marker object to be used at the right
#'   hand side of the [select<-()] or the double bracket replacement
#'   functions, see examples below.
#'
#' @export
#' @examples
#' # Using `deleted()` to delete elements
#' json <- load_json(text = serialize_json(list(
#'   a = list(a1 = list(1,2,3), a2 = "string"),
#'   b = list(4, 5, 6),
#'   c = list(c1 = list("a", "b"))
#' )))
#'
#' select(json, list("a", "a1")) <- deleted()
#' json
#'
#' json[[list("a", "a2")]] <- deleted()
#' json

deleted <- function() {
  structure(
    list(),
    class = c("tsjson_action_delete", "tsjson_action", "list")
  )
}
