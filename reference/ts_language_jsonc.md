# Tree sitter language object for JSONC

Use this function with
[`tsitter::ts_tree_new()`](https://r-lib.github.io/tsitter/reference/ts_tree_new.html)
to create a tree-sitter tree for a JSONC document.

## Usage

``` r
ts_language_jsonc()
```

## The JSON grammar

The grammar has the following node types. I included some less important
nodes in the subsection of other nodes that they are related to.

Comments may appear between any tokens, but they are not part of the
grammar.

Use the [bracket
operator](https://r-lib.github.io/tsitter/reference/ts_tree-brackets.html),
[`tsitter::ts_tree_dom()`](https://r-lib.github.io/tsitter/reference/ts_tree_dom.html)
and
[`tsitter::ts_tree_ast()`](https://r-lib.github.io/tsitter/reference/ts_tree_ast.html)
to explore the parse tree of a JSON document.

### `document`

\#' A document is a single value.

### Values

A value is one of:

- `object`,

- `array`,

- `number`,

- `string`,

- `true`,

- `false`,

- `null`.

### `object` / `pair`

An `object` is a sequence of

- `{`,

- zero or more `pair` nodes, separated by `,` nodes, trailing commas are
  allowed,

- `}`.

A pair is a series of

- a key, a `string` node,

- `:`,

- a value (see above).

### `array`

An `array` is a sequence of

- `[`,

- zero or more values (see above), separated by `,` nodes, trailing
  commas are allowed,

- `]`.

### `number`

An integer or floating point number. Minus sign is part of the number.
Scientific notation is supported.

### `string` / `string_content` / `escape_sequence`

A string is a sequence of

- a starting double quote (`"`),

- zero or more `string_content` or `escape_sequence` nodes,

- an ending double quote (`"`).

### `true` / `false` / `null`

The literals `true`, `false`, and `null`.

\[\`,

- zero or more values (see above), separated by `,` nodes, trailing
  commas are allowed,

- \`\]:
  R:%60,%0A-%20zero%20or%20more%20values%20(see%20above),%20separated%20by%20%60,%60%20nodes,%20trailing%0A%20%20commas%20are%20allowed,%0A-%20%60
