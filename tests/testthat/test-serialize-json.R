test_that("serialize_json", {
  expect_snapshot({
    serialize_json(NULL)
    serialize_json(TRUE)
    serialize_json(FALSE)
    serialize_json(1L)
    serialize_json(1.1)
    serialize_json(.25)
  })

  expect_snapshot({
    serialize_json(list())
    writeLines(serialize_json(list(1)))
    writeLines(serialize_json(list(1, 2, 3)))
    writeLines(serialize_json(list(1, list(21, 22), 3)))
    writeLines(serialize_json(list(1, list(a = 1, b = 2), 3)))
  })

  expect_snapshot({
    writeLines(serialize_json(structure(list(), names = character())))
    writeLines(serialize_json(list(a = 1)))
    writeLines(serialize_json(list(a = 1, b = 2, c = 3)))
    writeLines(serialize_json(list(a = 1, b = list(21, 22), c = 3)))
    writeLines(serialize_json(list(a = 1, b = list(b1 = 21, b2 = 22), c = 3)))
  })
})

test_that("serialize_json collapse", {
  expect_snapshot({
    (txt <- serialize_json(list(a = 1, b = 2), collapse = TRUE))
    writeLines(txt)
  })
})

test_that("serialize_json format", {
  expect_snapshot({
    writeLines(serialize_json(list(1, 2, 3), format = "compact"))
    writeLines(serialize_json(list(a = 1, b = 2), format = "compact"))
  })

  expect_snapshot({
    writeLines(serialize_json(list(1, 2, 3), format = "oneline"))
    writeLines(serialize_json(list(a = 1, b = 2), format = "oneline"))
  })
})

test_that("serialize_json file", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  serialize_json(list(a = 1, b = list(b1 = 21, b2 = 22), c = 3), file = tmp)
  expect_snapshot({
    writeLines(readLines(tmp))
  })
})
