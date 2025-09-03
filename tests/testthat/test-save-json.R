test_that("save_json", {
  json <- load_json(
    text = serialize_json(list(a = list(1, 2, 3), b = list(b1 = "foo")))
  )

  tmpdir <- tempfile()
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  mkdirp(tmpdir)
  tmp <- file.path(tmpdir, "test.json")
  save_json(json, file = tmp)

  expect_snapshot({
    load_json(tmp)
  })

  expect_snapshot(error = TRUE, {
    save_json(json)
  })

  save_json(delete_selections(load_json(tmp)[["a"]]))
  expect_snapshot({
    load_json(tmp)
  })

  expect_snapshot({
    save_json(json, file = stdout())
  })

  tmp2 <- file.path(tmpdir, "bin.json")
  out <- file(tmp2, open = "wb")
  on.exit(try(close(out), silent = TRUE), add = TRUE)
  save_json(json, file = out)
  close(out)
  expect_snapshot({
    load_json(tmp2)
  })
})
