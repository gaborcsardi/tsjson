test_that("unserialize_selected", {
  json <- load_json(text = '{ "a": 1, "b": [1, 2, 3] }')
  expect_snapshot({
    json |> unserialize_selected()
  })
})

test_that("unserialize_false", {
  json <- load_json(text = '{ "a": false, "b": [1, 2, 3] }')
  expect_snapshot({
    json |> select("a") |> unserialize_selected()
  })
})
