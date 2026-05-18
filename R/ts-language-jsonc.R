#' Tree sitter language object for JSONC
#'
#' Use this function with [tsitter::ts_tree_new()] to create a tree-sitter
#' tree for a JSONC document.
#'
#' @section The JSON grammar:
#'
#' The grammar has the following node types. I included some less important
#' nodes in the subsection of other nodes that they are related to.
#'
#' Comments may appear between any tokens, but they are not part of the
#' grammar.
#'
#' Use the [bracket operator][tsitter::ts_tree-brackets],
#' [tsitter::ts_tree_dom()] and [tsitter::ts_tree_ast()] to explore the parse tree
#' of a JSON document.
#'
#' ## `document`
#'
#' #' A document is a single value.
#'
#' ## Values
#'
#' A value is one of:
#' - `object`,
#' - `array`,
#' - `number`,
#' - `string`,
#' - `true`,
#' - `false`,
#' - `null`.
#'
#' ## `object` / `pair`
#'
#' An `object` is a sequence of
#' - `{`,
#' - zero or more `pair` nodes, separated by `,` nodes, trailing commas
#'   are allowed,
#' - `}`.
#'
#' A pair is a series of
#' - a key, a `string` node,
#' - `:`,
#' - a value (see above).
#'
#' ## `array`
#'
#' An `array` is a sequence of
#' - `[`,
#' - zero or more values (see above), separated by `,` nodes, trailing
#'   commas are allowed,
#' - `]`.
#'
#' ## `number`
#'
#' An integer or floating point number. Minus sign is part of the number.
#' Scientific notation is supported.
#'
#' ## `string` / `string_content` / `escape_sequence`
#'
#' A string is a sequence of
#' - a starting double quote (`"`),
#' - zero or more `string_content` or `escape_sequence` nodes,
#' - an ending double quote (`"`).
#'
#' ## `true` / `false` / `null`
#'
#' The literals `true`, `false`, and `null`.
#'
#' @export

ts_language_jsonc <- function() {
  .Call(c_ts_language_jsonc)
}
