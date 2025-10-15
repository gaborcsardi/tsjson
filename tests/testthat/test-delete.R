test_that("delete_selected comment is deleted", {
  json <- load_json(text = "{ \"a\": //comment\ntrue, \"b\": [1, 2, 3] }")
  expect_snapshot({
    json |> select("a") |> delete_selected()
  })
})

test_that("delete_selected comment is preserved", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3]\n//comment\n}")
  expect_snapshot({
    json |> select("a") |> delete_selected()
  })
})

test_that("delete_selected nothing to delete", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  expect_snapshot({
    json |> select("c") |> delete_selected()
  })
})

test_that("delete_selected all elements from an array", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1] }")
  expect_snapshot({
    json |> select("b", 1) |> delete_selected()
  })
})

test_that("delete_selected first elements from an array", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  expect_snapshot({
    json |> select("b", 1:2) |> delete_selected()
  })
})

test_that("delete_selected middle of an array", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3, 4] }")
  expect_snapshot({
    json |> select("b", 2:3) |> delete_selected()
  })
})

test_that("delete_selected last elements of an array", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3, 4] }")
  expect_snapshot({
    json |> select("b", 3:4) |> delete_selected()
  })
})

test_that("delete_selected whitespace of last element is kept", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3, 4] \n }")
  expect_snapshot({
    json |> select("b") |> delete_selected()
  })
})
