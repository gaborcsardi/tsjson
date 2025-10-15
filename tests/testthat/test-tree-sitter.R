test_that("sexpr_json", {
  expect_snapshot({
    sexpr_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  })
})

test_that("sexpr_json errors", {
  expect_snapshot(error = TRUE, {
    sexpr_json()
    sexpr_json(text = "foo", file = "bar")
  })
})

test_that("sexpr_json from a file", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('{ "a": true, "b": [1, 2, 3] }', tmp)
  expect_snapshot({
    sexpr_json(file = tmp)
  })
})

test_that("token_table", {
  expect_snapshot({
    token_table(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  })
})

test_that("token_table errors", {
  expect_snapshot(error = TRUE, {
    token_table()
    token_table(text = "foo", file = "bar")
  })
})

test_that("token_table from a file", {
  testthat::local_reproducible_output(width = 500)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('{ "a": true, "b": [1, 2, 3] }', tmp)
  expect_snapshot({
    token_table(file = tmp)
  })
})

test_that("syntax_tree_json", {
  expect_snapshot({
    syntax_tree_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
    text <- "{\n  \"a\": true,\n  \"b\": [\n    1,\n    2,\n    3\n  ]\n}"
    writeLines(text)
    syntax_tree_json(text = text)
  })
})

test_that("syntax_tree_json with hyperlinks", {
  withr::local_options(cli.hyperlink = TRUE)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  text <- "{\n  \"a\": true,\n  \"b\": [\n    1,\n    2,\n    3\n  ]\n}"
  writeLines(text, tmp)
  expect_snapshot(
    {
      syntax_tree_json(file = tmp)
    },
    transform = redact_tempfile
  )
})

test_that("query_json", {
  testthat::local_reproducible_output(width = 500)
  txt <- "{ \"a\": 1, \"b\": \"foo\", \"c\": 20 }"
  expect_snapshot({
    json <- load_json(text = txt) |> format_selected()
    json
    query_json(text = txt, query = "((pair value: (number) @num))")
  })
})

test_that("query_json errors", {
  expect_snapshot(error = TRUE, {
    query_json()
    query_json(text = "foo", file = "bar")
  })
})

test_that("query_json from a file", {
  testthat::local_reproducible_output(width = 500)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('{ "a": 1, "b": "foo", "c": 20 }', tmp)
  expect_snapshot({
    query_json(file = tmp, query = "((pair value: (number) @num))")
  })
})
