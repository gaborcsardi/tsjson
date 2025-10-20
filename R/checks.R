is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_string <- function(x, na = FALSE) {
  if (na) {
    is.character(x) && length(x) == 1
  } else {
    is.character(x) && length(x) == 1 && !is.na(x)
  }
}

is_count <- function(x, positive = FALSE) {
  limit <- if (positive) 1L else 0L
  is.numeric(x) &&
    length(x) == 1 &&
    !is.na(suppressWarnings(as.integer(x))) &&
    suppressWarnings(as.integer(x)) == x &&
    !is.na(x) &&
    x >= limit
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

as_count <- function(
  x,
  positive = FALSE,
  null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is.null(x) && null) {
    return(x)
  }
  if (is_count(x, positive = positive)) {
    return(as.integer(x))
  }

  if (is_string(x)) {
    xi <- suppressWarnings(as.integer(x))
    if (is_count(xi, positive = positive)) {
      return(xi)
    }
  }

  limit <- if (positive) 1L else 0L
  if (is.numeric(x) && length(x) != 1) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be an integer scalar, not a vector."
    ))
  } else if (is.numeric(x) && length(x) == 1 && is.na(x)) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must not be `NA`."
    ))
  } else if (is.numeric(x) && length(x) == 1 && !is.na(x) && x < limit) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be \\
      {if (positive) 'positive' else 'non-negative'}."
    ))
  } else {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be a \\
      {if (positive) 'positive' else 'non-negative'} integer scalar, \\
      but it is {typename(x)}."
    ))
  }
}

as_choice <- function(
  x,
  choices,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_string(x) && tolower(x) %in% choices) {
    return(tolower(x))
  }

  cchoices <- paste0("'", choices, "'", collapse = ", ")
  if (is_string(x)) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be one of {cchoices}, but it is '{x}'."
    ))
  } else {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be a string scalar, one of \\
       {cchoices}, but it is {typename(x)}."
    ))
  }
}

opt_allow_empty_content_default <- function() {
  TRUE
}

opt_allow_comments_default <- function() {
  TRUE
}

opt_format_default <- function() {
  "pretty"
}

opt_indent_width_default <- function() {
  4L
}

opt_indent_style_default <- function() {
  "space"
}

#' tsjson options
#'
#' Options that control the behavior of tsjson functions.
#'
#' ## Parsing options:
#'
#' * `allow_empty_content`: logical, whether to allow empty JSON documents.
#'   Default is `TRUE`.
#' * `allow_comments`: logical, whether to allow comments in JSON documents.
#'   Default is `TRUE`.
#'
#' ## Formatting options:
#'
#' * `format`: Formatting style, one of:
#'   - `"pretty"`: arrays and objects are formatted in multiple lines,
#'   - `"compact"`: format everything without whitespace,
#'   - `"oneline"`: format everything without newlines, but include
#'     whitespace after commas, colons, opening brackets and braces, and
#'     before closing brackets and braces.
#' * `indent_width`: integer, the number of spaces to use for indentation
#'   when `indent_style` is `"space"`. Default is `4`.
#' * `indent_style`: string, either `"space"` or `"tab"`, the type of
#'   indentation to use. Default is `"space"`.
#'
#' @name tsjson_options
NULL

as_tsjson_options <- function(
  x,
  auto_format = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  nms <- c(
    "allow_empty_content",
    "allow_comments",
    "format",
    "indent_width",
    "indent_style"
  )
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

    x[["format"]] <- as_choice(
      x[["format"]] %||% opt_format_default(),
      choices = c("pretty", "compact", "oneline", if (auto_format) "auto"),
      arg = as_caller_arg(substitute(
        x[["format"]],
        list(x = arg[[1]])
      )),
      call = call
    )

    x[["indent_width"]] <- as_count(
      x[["indent_width"]] %||% opt_indent_width_default(),
      arg = as_caller_arg(substitute(
        x[["indent_width"]],
        list(x = arg[[1]])
      )),
      call = call
    )

    x[["indent_style"]] <- as_choice(
      x[["indent_style"]] %||% opt_indent_style_default(),
      choices = c("space", "tab"),
      arg = as_caller_arg(substitute(
        x[["indent_style"]],
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

  bad <- paste0("`", unique(setdiff(names(x), nms)), "`")
  good <- paste0("`", nms, "`")
  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` contains unknown tsjson \\
    option{plural(length(bad))}: {collapse(bad)}. Known tsjson options \\
    are: {collapse(good)}."
  ))
}
