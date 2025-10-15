test_that("get_language error", {
  expect_snapshot(error = TRUE, {
    .Call(c_s_expr, "input", 1L, NULL)
  })
})

test_that("get_ranges", {
  text <- "{ \"a\": true, \"b\": [1, 2, 3] }"
  text1 <- paste0(
    substr(text, 1, 10),
    strrep("-", 100),
    substr(text, 11, nchar(text))
  )
  ranges <- data.frame(
    start_row = c(1L, 1L),
    start_col = c(1L, 111L),
    end_row = c(1L, 1L),
    end_col = c(10L, nchar(text1)),
    start_byte = c(1L, 111L),
    end_byte = c(10L, nchar(text1))
  )
  expect_snapshot({
    sexpr_json(text = text1, ranges = ranges)
  })
  expect_snapshot(error = TRUE, {
    sexpr_json(text = text1, ranges = list(c(1L, 10L), c(111L, nchar(text1))))
  })
  badranges <- ranges
  badranges[[1]] <- as.numeric(badranges[[1]])
  expect_snapshot(error = TRUE, {
    sexpr_json(text = text1, ranges = badranges)
  })

  # overlapping ranges are invalid
  badranges2 <- data.frame(
    start_row = c(1L, 1L),
    start_col = c(1L, 11L),
    end_row = c(1L, 1L),
    end_col = c(15L, nchar(text)),
    start_byte = c(1L, 11L),
    end_byte = c(15L, nchar(text))
  )
  expect_snapshot(error = TRUE, {
    sexpr_json(text = text, ranges = badranges2)
  })
})

test_that("token_table", {
  text <- "{ \"a\": true, \"b\": [1, 2, 3] }"
  badranges2 <- data.frame(
    start_row = c(1L, 1L),
    start_col = c(1L, 11L),
    end_row = c(1L, 1L),
    end_col = c(15L, nchar(text)),
    start_byte = c(1L, 11L),
    end_byte = c(15L, nchar(text))
  )
  expect_snapshot(error = TRUE, {
    token_table(text = text, ranges = badranges2)
  })
})

test_that("check_predicate_eq", {
  testthat::local_reproducible_output(width = 500)
  text <- "{ \"a\": 1, \"b\": \"foo\", \"c\": 20 }"
  expect_snapshot({
    query_json(
      text = text,
      query = "((pair key: (string (string_content) @key) (#eq? @key \"a\")))"
    )
  })
})

test_that("check_predicate_eq 2", {
  testthat::local_reproducible_output(width = 500)
  text <- "{ \"a\": 1, \"b\": \"foo\", \"x\": \"x\", \"c\": 20 }"
  expect_snapshot({
    query_json(
      text = text,
      query = "((pair key: (string (string_content) @key)
                 value: (string (string_content) @val)
                 (#eq? @key @val)
               ))"
    )
  })
})
