#' @export

update_selections <- function(
  json,
  new,
  format = c("auto", "pretty", "compact", "oneline")
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
    attr(json, "selection") <- selection[1:(ptr - 1L)]
    return(insert_at_selections(json, new[[1]], key = names(new)))
  }

  deleted <- select
  while (TRUE) {
    deleted2 <- unique(c(deleted, unlist(json$children[deleted])))
    if (length(deleted2) == length(deleted)) {
      break
    }
    deleted <- deleted2
  }

  # keep select nodes to inject the new elements
  code <- serialize_json(new, collapse = TRUE, format = "compact")
  json$code[select] <- NA_character_
  json$tws[select] <- code
  really_deleted <- setdiff(deleted, select)
  if (length(really_deleted)) {
    json <- json[-really_deleted, ]
  }

  parts <- c(rbind(json$code, json$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- load_json(text = text)
  attr(new, "file") <- attr(json, "file")

  new
}
