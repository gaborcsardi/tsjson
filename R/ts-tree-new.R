#' Create a new tree-sitter tree for a JSONC document
#'
#' @usage
#' \method{ts_tree_new}{ts_language_jsonc}(
#'   language,
#'   file = NULL,
#'   text = NULL,
#'   ranges = NULL,
#'   fail_on_parse_error = TRUE,
#'   options = NULL,
#'   ...
#' )
#' @param language
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_new_param_language", "tsjsonc")}
#' @param file
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_new_param_file", "tsjsonc")}
#' @param text
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_new_param_text", "tsjsonc")}
#' @param ranges
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_new_param_ranges", "tsjsonc")}
#' @param fail_on_parse_error
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_new_param_fail_on_parse_error",
#'   "tsjsonc")}
#' @param options Named list of formatting options, see
#'   [tsjsonc options][tsjsonc_options].
#' @param ... Reserved for future use.
#' @return
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_new_return", "tsjsonc")}
#'
#' @description
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_new_description", "tsjsonc")}
#' @details
#' \if{text}{This is the S3 method of the [tsitter::ts_tree_new()] generic.}
#'
#' \eval{tsitter:::doc_extra()}
#' \eval{tsitter:::doc_insert("tsitter::ts_tree_new_details", "tsjsonc")}
#'
#' @ts ts_tree_new_examples JSONC examples
#'
#' ```{asciicast}
#' jsonc <- tsitter::ts_tree_new(
#'   tsjsonc::ts_language_jsonc(),
#'   text = "{ \"a\": true, // comment\n \"b\": [1, 2, 3], }"
#' )
#' jsonc
#' ```
#'
#' @aliases ts_tree_jsonc
#' @export

ts_tree_new.ts_language_jsonc <- function(
  language,
  file = NULL,
  text = NULL,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  options = NULL,
  ...
) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tsjsonc_options(options)
  # TODO check empty dots

  tree <- NextMethod()

  # to work around a TS bug, these are not terminal nodes
  tree$code[tree$type %in% c("object", "array")] <- NA_character_

  # TODO make this a proper error, mark the position of the comment(s)
  if (!options[["allow_comments"]]) {
    comments <- which(tree$type == "comment")
    if (length(comments) > 0) {
      stop(ts_cnd(
        "The JSON document contains comments, and this is not allowed. \\
         To allow comments, set the `allow_comments` option to `TRUE`."
      ))
    }
  }

  top <- tree$children[[1]]
  top <- top[tree$type[top] != "comment"]
  if (!options[["allow_empty_content"]] && length(top) == 0) {
    stop(ts_cnd(
      "The JSON document is empty, and this is not allowed. \\
       To allow this, set the `allow_empty_content` option to `TRUE`."
    ))
  }

  # TODO make this a proper error, mark the position of the trailing comma(s)
  if (!options[["allow_trailing_comma"]]) {
    commas <- which(tree$type == ",")
    trailing <- map_lgl(commas, function(c) {
      siblings <- tree$children[[tree$parent[c]]]
      c == siblings[length(siblings) - 1L]
    })
    if (any(trailing)) {
      stop(ts_cnd(
        "The JSON document contains trailing commas, and this is not allowed. \\
         To allow trailing commas, set the `allow_trailing_comma` option to \\
         `TRUE`."
      ))
    }
  }

  # create the DOM tree
  tree <- add_dom(tree)

  tree
}

dom_types <- c(
  "null",
  "true",
  "false",
  "string",
  "number",
  "array",
  "object",
  "document"
)


add_dom <- function(tree) {
  tree$dom_children <- vector("list", nrow(tree))
  tree$dom_parent <- rep(NA_integer_, nrow(tree))
  tree$dom_name <- rep(NA_character_, nrow(tree))
  tree$dom_type <- ifelse(tree$type %in% dom_types, tree$type, NA_character_)

  for (i in seq_len(nrow(tree))) {
    if (!tree$type[i] %in% c("document", "object", "array")) {
      next
    }

    chdn <- tree$children[[i]]
    chdn <- chdn[!tree$type[chdn] %in% c("comment", "[", "]", "{", "}", ",")]

    if (tree$type[i] == "object") {
      nms <- rep("", length(chdn))
      for (ci in seq_along(chdn)) {
        gchdn <- tree$children[[chdn[ci]]]
        chdn[ci] <- gchdn[3]
        nms[ci] <- unserialize_string(tree, gchdn[1])
      }
      names(chdn) <- tree$dom_name[chdn] <- nms
    }

    tree$dom_children[[i]] <- chdn
    tree$dom_parent[chdn] <- i
  }

  tree
}
