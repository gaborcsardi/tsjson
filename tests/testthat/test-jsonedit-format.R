test_that("formatting retains comments", {
  text <- '
  {
    // a comment

  "a":1, // another one
    "b": {
      "c":2
    }
  } // trailing
  '
  expect_snapshot(writeLines(format_json(text = text)))
})

# test_that("`eol` only applies when we don't know the eol in `text`", {
#   skip("does not apply, we return a vector of lines now")
#   options <- list(eol = "\r\n")

#   text <- '{"a":1}\n'
#   expect_identical(
#     format_json(text = text, options = options),
#     strsplit('{\n    "a": 1\n}\n', "\n")[[1]]
#   )

#   text <- '{"a":1}'
#   expect_identical(
#     format_json(text = text, options = options),
#     strsplit('{\r\n    "a": 1\r\n}\r\n', "\n")[[1]]
#   )
# })

test_that("`indent_width` works", {
  options <- list(indent_width = 2)

  text <- '{"a":1}\n'
  expect_identical(
    format_json(text = text, options = options),
    strsplit('{\n  "a": 1\n}\n', "\n")[[1]]
  )
})

test_that("`indent_style` works", {
  options <- list(indent_style = "tab")

  text <- '{"a":1}\n'
  expect_identical(
    format_json(text = text, options = options),
    strsplit('{\n\t"a": 1\n}\n', "\n")[[1]]
  )
})

# test_that("`insert_final_newline` works", {
#   skip("does not apply, we return a vector of lines now")
#   options <- list(insert_final_newline = FALSE)

#   text <- '{"a":1}\n'
#   expect_identical(
#     format_json(text = text, options = options),
#     strsplit('{\n    "a": 1\n}', "\n")[[1]]
#   )
# })

# test_that("`insert_final_newline` works", {
#   skip("does not apply, we return a vector of lines now")
#   # Removes if there
#   text <- '{"a":1}\n'
#   options <- list(insert_final_newline = FALSE)
#   expect_identical(
#     format_json(text = text, options = options),
#     strsplit('{\n    "a": 1\n}', "\n")[[1]]
#   )

#   # Adds if missing
#   text <- '{"a":1}'
#   options <- list(insert_final_newline = TRUE)
#   expect_identical(
#     format_json(text = text, options = options),
#     strsplit('{\n    "a": 1\n}\n', "\n")[[1]]
#   )
# })
