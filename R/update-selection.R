#' Replace selected JSON elements with a new element
#'
#' Replace all selected elements with a new element. If `json` has no
#' selection then the whole document is replaced. If `json` has an empty
#' selection, then nothing happens.
#'
#' @param json tsjson object.
#' @param new R object that will be serialized to JSON (using
#'   [serialize_json()]) and inserted in place of the selected JSON
#'   elements.
#' @param format How to format `new`. See [format_selected()].
#' @return The updated tsjson object
#'
#' @export
#' @examples
#' json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
#' json
#'
#' json |> select("a") |> update_selected(list("new", "element"))

update_selected <- function(
  json,
  new,
  format = c("pretty", "compact", "oneline")
) {
  format <- match.arg(format)
  selection <- get_selection(json)
  ptr <- length(selection)
  select <- selection[[ptr]]$nodes

  # if no selection, then maybe this is an insert
  if (length(select) == 0) {
    while (length(selection[[ptr]]$nodes) == 0) {
      slt <- selection[[ptr]]$selector
      # only if characters
      if (inherits(slt, "tsjson_selector") || !is.character(slt)) {
        return(json)
      }
      ptr <- ptr - 1L
      new <- structure(
        replicate(length(slt), new, simplify = FALSE),
        names = slt
      )
    }
    attr(json, "selection") <- selection[1:ptr]
    return(insert_into_selected(json, new[[1]], key = names(new)))
  }

  fmt <- replicate(
    length(select),
    serialize_json(new, collapse = FALSE, format = format),
    simplify = FALSE
  )

  # keep original indentation at the start row
  for (i in seq_along(select)) {
    sel1 <- select[i]
    prevline <- rev(which(json$end_row == json$start_row[sel1] - 1))[1]
    ind0 <- sub("^.*\n", "", json$tws[prevline])
    if (!is.na(prevline)) {
      fmt[[i]] <- paste0(c("", rep(ind0, length(fmt[[i]]) - 1L)), fmt[[i]])
    }
  }

  subtrees <- lapply(select, get_subtree, json = json, with_root = FALSE)
  deleted <- unique(unlist(subtrees))

  # need to keep the trailing ws of the last element
  lasts <- map_int(subtrees, max_or_na)
  tws <- json$tws[lasts]
  json$code[deleted] <- NA_character_
  json$tws[deleted] <- NA_character_

  # keep select nodes to inject the new elements
  json$code[select] <- paste0(
    map_chr(fmt, paste, collapse = "\n"),
    ifelse(is.na(tws), "", tws)
  )
  json$tws[select] <- NA_character_

  parts <- c(rbind(json$code, json$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- load_json(text = text)
  attr(new, "file") <- attr(json, "file")

  new
}
