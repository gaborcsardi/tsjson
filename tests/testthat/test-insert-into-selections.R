test_that("insert_into_selected", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  expect_snapshot({
    json |>
      select("b") |>
      insert_into_selected("foo", at = 1, options = list(format = "auto"))
  })
})

test_that("insert_into_selected with empty selection", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  expect_snapshot({
    json |>
      select("new") |>
      insert_into_selected("foo", options = list(format = "auto"))
  })
})

test_that("insert_into_selected multi-line array is pretty", {
  json <- load_json(text = "{ \"a\": true, \"b\": [\n  1,\n  2,\n  3\n] }")
  expect_snapshot({
    json |>
      select("b") |>
      insert_into_selected(list(a = 1, b = 2), options = list(format = "auto"))
  })
})

test_that("insert_into_selected with compact array is compact", {
  json <- load_json(text = "{ \"a\":true, \"b\":[1,2,3] }")
  expect_snapshot({
    json |>
      select("b") |>
      insert_into_selected(list(1, 2), options = list(format = "auto"))
  })
})

test_that("insert_into_selected document", {
  json <- load_json(text = "")
  expect_snapshot({
    json |>
      insert_into_selected(list(a = 1, b = 2), options = list(format = "auto"))
  })
})

test_that("insert_into_selected object", {
  json <- load_json(text = "{ \"a\": { } }")
  expect_snapshot({
    json |>
      select("a") |>
      insert_into_selected(42, key = "b", options = list(format = "auto"))
  })
  json <- load_json(text = "{ \"a\": { \"b\": 42 } }")
  expect_snapshot({
    json |>
      select("a") |>
      insert_into_selected(43, key = "c", options = list(format = "auto"))
  })
})

test_that("insert_into_selected force formatting", {
  json <- load_json(text = "{ \"a\":true, \"b\":[1,2,3] }")
  expect_snapshot({
    json |>
      select("b") |>
      insert_into_selected(list(1, 2), options = list(format = "pretty"))
  })
})

test_that("insert_into_document errors", {
  json <- load_json(text = "{ \"a\":true, \"b\":[1,2,3] }")
  expect_snapshot(error = TRUE, {
    json |> insert_into_document("true", "pretty")
  })
})

test_that("insert_into_selected adds newline if needed", {
  json <- load_json(text = "// comment")
  expect_snapshot({
    json |>
      insert_into_selected(list(a = 1, b = 2), options = list(format = "auto"))
  })
  json <- load_json(text = "// comment\n// comment2")
  expect_snapshot({
    json |>
      insert_into_selected(list(a = 1, b = 2), options = list(format = "auto"))
  })
})

test_that("insert_into_selected invalid index", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  expect_snapshot(error = TRUE, {
    json |>
      select("b") |>
      insert_into_selected("foo", at = "bar", options = list(format = "auto"))
  })
})

test_that("insert_into_selected insert into empty array", {
  json <- load_json(text = "{ \"a\": true, \"b\": [] }")
  expect_snapshot({
    json |>
      select("b") |>
      insert_into_selected("foo", options = list(format = "auto"))
  })
})

test_that("insert_into_selected insert at beginning of array", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1] }")
  expect_snapshot({
    json |>
      select("b") |>
      insert_into_selected("foo", at = 0, options = list(format = "auto"))
  })
})

test_that("insert_into_selected insert into object by key", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1] }")
  expect_snapshot({
    json |>
      insert_into_selected(
        "val",
        key = "key",
        at = "a",
        options = list(format = "auto")
      )
  })
})

test_that("insert_into_selected insert into object by non-existing key", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1] }")
  expect_snapshot({
    json |>
      insert_into_selected(
        "val",
        key = "key",
        at = "nope",
        options = list(format = "auto")
      )
  })
})

test_that("insert_into_selected insert into object at be beginning", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1] }")
  expect_snapshot({
    json |>
      insert_into_selected(
        "val",
        key = "key",
        at = 0,
        options = list(format = "auto")
      )
  })
})
