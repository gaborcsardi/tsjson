is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_named <- function(x) {
  nms <- names(x)
  length(x) == length(nms) && !anyNA(nms) && all(nms != "")
}

as_flag <- function(x, null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (null && is.null(x)) {
    return(x)
  }
  if (is_flag(x)) {
    return(x)
  }

  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` must a flag (logical scalar), but it is \\
     {typename(x)}."
  ))
}

opt_allow_empty_content_default <- function() {
  TRUE
}

opt_allow_comments_default <- function() {
  TRUE
}

#' tsjson options
#'
#' Options that control the behavior of tsjson functions.
#'
#' * `allow_empty_content`: logical, whether to allow empty JSON documents.
#'   Default is `TRUE`.
#' * `allow_comments`: logical, whether to allow comments in JSON documents.
#'   Default is `TRUE`.
#' @name tsjson_options
NULL

as_tsjson_options <- function(
  x,
  arg = caller_arg(x),
  cell = caller_env()
) {
  nms <- c("allow_empty_content", "allow_comments")
  if ((is.list(x) || is.null(x)) && is_named(x) && all(names(x) %in% nms)) {
    force(arg)

    x[["allow_empty_content"]] <- as_flag(
      x[["allow_empty_content"]] %||% opt_allow_empty_content_default(),
      arg = as_caller_arg(substitute(
        x[["allow_empty_content"]],
        list(x = arg[[1]])
      )),
      call = call
    )

    x[["allow_comments"]] <- as_flag(
      x[["allow_comments"]] %||% opt_allow_comments_default(),
      arg = as_caller_arg(substitute(
        x[["allow_comments"]],
        list(x = arg[[1]])
      )),
      call = call
    )

    return(x)
  }

  if (!is.list(x) && !is.null(x)) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be a named list of tsjson options \\
       (see `?tsjson_options`), but it is {typename(options)}."
    ))
  }

  if (!is_named(x)) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be a named list of tsjson options \\
       (see `?tsjson_options`), but not all of its entries are named."
    ))
  }

  bad <- unique(setdiff(names(x), nms))
  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` contains unknown tsjson \\
    option{plural(length(bad))}: {collapse(bad)}. Known tsjson options \\
    are: {collapse(nms)}."
  ))
}
