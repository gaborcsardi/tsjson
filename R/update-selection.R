#' @export

update_selections <- function(json, new) {
  select <- get_selection(json)

  if (length(select) == 0) {
    return(json)
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
  code <- serialize_json(new, collapse = TRUE)
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
