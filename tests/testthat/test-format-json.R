test_that("format_json", {
  text <- serialize_json(
    options = list(format = "compact"),
    list(
      a = 1,
      b = list(b1 = 21, b2 = 22),
      c = 3,
      d = list(1, 2, 3)
    )
  )

  expect_snapshot({
    writeLines(format_json(text = text))
  })
})

test_that("format_selected", {
  text <- serialize_json(
    options = list(format = "compact"),
    list(
      a = 1,
      b = list(b1 = 21, b2 = 22),
      c = 3,
      d = list(1, 2, 3)
    )
  )
  json <- load_json(text = text)

  expect_snapshot({
    format_selected(json)
    format_selected(select(json, "a"))
    format_selected(select(json, "b"))
  })
  json <- format_selected(json)
  expect_snapshot({
    format_selected(select(json, "b"))
  })
})

test_that("format_selected null, true, false, string, comment", {
  text <- serialize_json(
    options = list(format = "compact"),
    list(
      a = NULL,
      b = TRUE,
      c = FALSE,
      d = list("a", "b", "c")
    )
  )
  json <- load_json(text = text)
  expect_snapshot({
    format_selected(json)
  })
  json <- load_json(text = "{ // comment\n  \"a\": // comment\n    null\n}")
  expect_snapshot({
    format_selected(json)
  })
})

test_that("format_selected empty array", {
  text <- serialize_json(
    options = list(format = "compact"),
    list(
      a = list(),
      b = TRUE
    )
  )
  json <- load_json(text = text)
  expect_snapshot({
    format_selected(json)
  })
})

test_that("format_selected compact arrays", {
  text <- serialize_json(
    list(
      a = list(1, 2, 3),
      b = TRUE
    )
  )
  json <- load_json(text = text)
  expect_snapshot({
    json
    format_selected(json, options = list(format = "compact"))
  })
})

test_that("format_selected oneline arrays", {
  text <- serialize_json(
    list(
      a = list(1, 2, 3),
      b = TRUE
    )
  )
  json <- load_json(text = text)
  expect_snapshot({
    json
    format_selected(json, options = list(format = "oneline"))
  })
})

test_that("format_selected empty object", {
  text <- serialize_json(
    options = list(format = "compact"),
    list(
      a = structure(list(), names = character()),
      b = TRUE
    )
  )
  json <- load_json(text = text)
  expect_snapshot({
    format_selected(json)
  })
})

test_that("format_selected drop comments in compact, oneline modes", {
  json <- load_json(text = "{ // comment\n  \"a\": // comment\n    null\n}")
  expect_snapshot({
    json
    format_selected(json, options = list(format = "compact"))
    format_selected(json, options = list(format = "oneline"))
  })
})
