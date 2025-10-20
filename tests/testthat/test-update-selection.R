test_that("update_selected", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  expect_snapshot({
    json |> select("a") |> update_selected(list("new", "element"))
  })
})

test_that("update_selected with empty selection can be an insert", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  expect_snapshot({
    upd <- json |>
      select("new", "element") |>
      update_selected(list("new", "value"), options = list(format = "pretty"))
    print(upd, n = Inf)
  })
})

test_that("updated_selected with empry non-character selection is noop", {
  json <- load_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
  expect_snapshot({
    upd <- json |>
      select("b", 10) |>
      update_selected(list("new", "value"))
    print(upd, n = Inf)
  })
})
