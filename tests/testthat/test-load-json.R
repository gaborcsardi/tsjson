test_that("load_json", {
  testthat::local_reproducible_output(width = 500)
  json <- load_json(text = "")
  expect_snapshot(json)

  json <- load_json(text = "[]")
  expect_snapshot(json)

  json <- load_json(text = "// comment\n[1,2,3]")
  expect_snapshot({
    json
    json[]
  })

  # from file
  tmpdir <- tempfile()
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  mkdirp(tmpdir)
  tmp <- file.path(tmpdir, "three.json")
  writeLines(c("// comment", "[1,2,3]"), tmp)
  json <- load_json(tmp)
  expect_snapshot({
    json
  })

  # leading whitespace
  json <- load_json(text = "\n\n   [1,2,3]\n")
  expect_snapshot({
    json
    json[]
  })
})

test_that("load_json with options", {
  testthat::local_reproducible_output(width = 500)
  json <- load_json(
    text = "// comment\n{ \"a\": 1 }",
    options = list(allow_comments = TRUE)
  )
  expect_snapshot({
    json
  })
})

test_that("load_json errors", {
  expect_snapshot(error = TRUE, {
    load_json()
    load_json(file = tempfile(), text = "foo")
  })
})

test_that("[.tsjson", {
  # tested above
  expect_true(TRUE)
})
