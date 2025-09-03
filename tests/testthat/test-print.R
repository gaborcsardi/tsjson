test_that("print.tsjson", {
  json <- load_json(
    text = serialize_json(list(a = list(1, 2, 3), b = list(b1 = "foo")))
  )
  expect_snapshot(json)
})

test_that("format_tsjson_noselection", {
  json <- load_json(
    text = serialize_json(list(a = list(1, 2, 3), b = list(b1 = "foo")))
  )
  expect_snapshot({
    json[["no-such-element"]]
  })
})

test_that("format_tsjson_noselection long document", {
  json <- load_json(
    text = serialize_json(list(a = as.list(letters)))
  )
  expect_snapshot({
    json
  })
})

test_that("format_tsjson_selection", {
  json <- load_json(
    text = serialize_json(list(a = list(1, 2, 3), b = list(b1 = "foo")))
  )
  expect_snapshot({
    json[["a"]]
    json[["a"]][[1:2]]
    json[[list("b", "b1")]]
  })
})

test_that("many selections", {
  json <- load_json(
    text = serialize_json(list(a = as.list(1:100)))
  )
  expect_snapshot({
    json[["a"]][[seq(2, 30, by = 2)]]
  })
})
