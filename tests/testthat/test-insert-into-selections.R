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

test_that("insert_into_array, comment is kept on same line", {
  json <- load_json(text = '{ "a": [1, 2 // comment\n]\n}')
  expect_snapshot({
    json
    json |> select("a") |> insert_into_selected(42, at = Inf)
    json |> select("a") |> insert_into_selected(42, at = 2)
  })
})

test_that("insert_into_array, multiple comments before comma", {
  json <- load_json(text = '{ "a": [1\n// comment1\n// comment2\n, 2]\n}')
  expect_snapshot({
    json
    json |> select("a") |> insert_into_selected(42, at = 1)
  })
})

test_that("insert_into_object, comment is kept on same line", {
  json <- load_json(text = '{ "a": 1, // comment\n  "b": 2\n}')
  expect_snapshot({
    json
    json |> insert_into_selected(42, key = "x", at = "a")
  })
})

test_that("insert_into_object, multiple comments before comma", {
  json <- load_json(text = '{ "a": 1\n// comment1\n// comment2\n, "b": 2\n}')
  expect_snapshot({
    json
    json |> insert_into_selected(42, at = "a", key = "x")
  })
})

test_that("insert_into_array, trailing comma and appending", {
  json <- load_json(text = "[1,2,3,//comment\n]")
  expect_snapshot({
    json
    json |> insert_into_selected(4)
  })
})

test_that("insert_into_object, trailing comma and appending", {
  json <- load_json(text = "{ \"a\": 1, \"b\": 2, \"c\": 3, // comment\n}")
  expect_snapshot({
    json
    json |> insert_into_selected(key = "d", 4)
  })
})
