#' @export

serialize_json <- function(obj, file = NULL, collapse = FALSE) {
  opts <- list(auto_unbox = TRUE, pretty = TRUE)
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
