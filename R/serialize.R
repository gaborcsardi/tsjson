serialize_json <- function(
  obj,
  file = NULL,
  collapse = FALSE,
  format = c("pretty", "compact", "oneline")
) {
  format <- match.arg(format)
  opts <- list(auto_unbox = TRUE, format = format)
  if (is.null(file)) {
    if (collapse) {
      tojson$write_str(obj, opts = opts)
    } else {
      tojson$write_lines(obj, opts = opts)
    }
  } else {
    tojson$write_file(obj, file, opts = opts)
  }
}
