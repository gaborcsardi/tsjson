test_that("can modify objects by name", {
  expect_snapshot(
    load_json(text = "{}") |>
      select("foo") |>
      update_selected(1)
  )
  expect_snapshot(
    load_json(text = "{}") |>
      select("foo") |>
      update_selected(1:2)
  )
  expect_snapshot(
    load_json(text = "{}") |>
      select("foo") |>
      update_selected(list(1, "x"))
  )
})

test_that("modification retains comments", {
  text <- '
{
    // a
    "foo": 1, // b
    "bar": [
        // c
        1,
        2, // d
        // e
        3
    ] // f
    // g
}
  '

  expect_snapshot(
    load_json(text = text) |> select("foo") |> update_selected(0)
  )

  expect_snapshot(
    load_json(text = text) |> select("bar", 2) |> update_selected(0)
  )
  expect_snapshot(
    print(
      load_json(text = text) |>
        select("bar") |>
        insert_into_selected(0, at = 2),
      n = 20
    )
  )
  return()
  expect_snapshot(
    load_json(text = text) |> select("new") |> update_selected(0)
  )
})

return()

test_that("can't modify non-object non-array parents", {
  expect_snapshot(error = TRUE, {
    text_modify("1", "foo", 0)
  })
  expect_snapshot(error = TRUE, {
    text_modify('"a"', "foo", 0)
  })
  expect_snapshot(error = TRUE, {
    text_modify("true", "foo", 0)
  })
  expect_snapshot(error = TRUE, {
    text_modify("null", "foo", 0)
  })
})
