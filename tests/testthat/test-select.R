test_that("get_selection, get_selected_nodes", {
  json <- load_json(text = "")
  expect_snapshot({
    get_selection(json)
    get_selection(json, default = FALSE)
    get_selected_nodes(json)
    get_selected_nodes(json, default = FALSE)
  })

  json <- load_json(text = "[]")
  expect_snapshot({
    get_selection(json)
    get_selection(json, default = FALSE)
    get_selected_nodes(json)
    get_selected_nodes(json, default = FALSE)
  })

  json <- load_json(text = "[1,2,3]")
  expect_snapshot({
    json
    json[[sel_all()]]
    get_selection(json[[sel_all()]])
    get_selected_nodes(json[[sel_all()]])
  })
})

test_that("select", {
  json <- load_json(
    text = serialize_json(list(
      a = 1,
      b = list(b1 = 21, b2 = 22),
      c = 3,
      d = list(1, 2, 3)
    ))
  )
  expect_snapshot({
    select(json)
    select(json, "a")
    select(json, c("a", "b"))
    select(json, "b", "b1")
    select(json, list("b", "b1"))
    select(json, "d", 1)
    select(json, "d", sel_all())
  })

  expect_snapshot(error = TRUE, {
    select(json, raw(0))
  })
})

test_that("deselect", {
  json <- load_json(
    text = serialize_json(list(a = 1, c = 3))
  )
  expect_snapshot({
    select(json, "a")
    deselect(select(json, "a"))
  })
})

test_that("[[.tdjson", {
  json <- load_json(
    text = serialize_json(list(
      a = 1,
      b = list(b1 = 21, b2 = 22),
      c = 3,
      d = list(1, 2, 3)
    ))
  )
  expect_snapshot({
    json[[]]
    json[["a"]]
    json[[c("a", "b")]]
    json[["b"]][["b1"]]
    json[[list("b", "b1")]]
    json[["d"]][[1]]
    json[["d"]][[sel_all()]]
  })

  expect_snapshot({
    json[["d"]][["nothing"]]
  })
})

test_that("[[<-.tsjson", {
  json <- load_json(
    text = serialize_json(list(
      a = 1,
      b = list(b1 = 21, b2 = 22),
      c = 3,
      d = list(1, 2, 3)
    ))
  )

  expect_snapshot({
    json[["a"]] <- 2
    json
  })

  expect_snapshot({
    json[[c("a", "c")]] <- TRUE
    json
  })

  expect_snapshot({
    json[["b"]][["b1"]] <- 100
    json
  })

  expect_snapshot({
    json[[list("b", "b1")]] <- 100
    json
  })
})

test_that("[[<-.tsjson empty doc", {
  json <- load_json(text = "")
  json[[]] <- list()
  expect_snapshot(json)

  json <- load_json(text = "")
  json[[]] <- structure(list(), names = character())
  expect_snapshot(json)
})

test_that("[[<-.tsjson deletion", {
  json <- load_json(
    text = serialize_json(list(
      a = 1,
      b = list(b1 = 21, b2 = 22),
      c = 3,
      d = list(1, 2, 3)
    ))
  )

  expect_snapshot({
    json[[c("a", "b")]] <- deleted()
    json
  })
})

test_that("sel_regex", {
  json <- load_json(
    text = serialize_json(list(
      a1 = 1,
      a2 = list(b1 = 21, b2 = 22),
      c = 3,
      a3 = list(1, 2, 3)
    ))
  )

  expect_snapshot({
    json[[sel_regex("^a")]]
  })

  # regex in array selects nothing
  json2 <- load_json(
    text = serialize_json(list(1, 2, 3))
  )
  expect_snapshot(
    json2[[sel_regex(".")]]
  )
})

test_that("sel_back", {
  json <- load_json(
    text = serialize_json(list(
      a = 1,
      b = list(b1 = 21, b2 = 22),
      c = 3,
      d = list(1, 2, 3)
    ))
  )

  expect_snapshot({
    json[[sel_back(1)]][[sel_back(2)]]
  })
})

test_that("sel_ids", {
  json <- load_json(
    text = serialize_json(list(
      a = 1,
      b = list(b1 = 21, b2 = 22),
      c = 3,
      d = list(1, 2, 3)
    ))
  )

  expect_snapshot({
    json[[sel_ids(26)]]
  })
})
