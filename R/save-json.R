#' @export

save_json <- function(json, file = NULL) {
  file <- file %||% attr(json, "file")
  if (is.null(file)) {
    stop(cnd(
      "Don't know which file to save JSON document to. You need to \\
       specify the `file` argument."
    ))
  }

  term <- which(!is.na(json$code))
  parts <- rbind(c("", json$code[term]), c(json$tws[1], json$tws[term]))
  writeLines(parts, con = file, sep = "")
}
