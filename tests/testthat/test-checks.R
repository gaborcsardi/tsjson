test_that("is_flag", {
  expect_true(is_flag(TRUE))
  expect_true(is_flag(FALSE))
  expect_false(is_flag(1))
  expect_false(is_flag("TRUE"))
  expect_false(is_flag(c(TRUE, FALSE)))
})

test_that("is_string", {
  expect_true(is_string("a"))
  expect_true(is_string(NA_character_, na = TRUE))

  expect_false(is_string(1))
  expect_false(is_string(TRUE))
  expect_false(is_string(c("a", "b")))
  expect_false(is_string(character()))
  expect_false(is_string(NA_character_))
})

test_that("is_count", {
  expect_true(is_count(1))
  expect_true(is_count(10L))
  expect_true(is_count(0))

  expect_false(is_count(-1))
  expect_false(is_count(1.5))
  expect_false(is_count("1"))
  expect_false(is_count(c(1, 2)))
  expect_false(is_count(NA_integer_))
  expect_false(is_count(0, positive = TRUE))
})

test_that("is_named", {
  expect_true(is_named(list(a = 1, b = 2)))
  expect_true(is_named(list(a = 1, b = NULL)))
  expect_true(is_named(list()))

  expect_false(is_named(list(1, 2)))
  expect_false(is_named(list(a = 1, 2)))
})

test_that("as_flag", {
  expect_null(as_flag(NULL, null = TRUE))
  expect_equal(as_flag(TRUE), TRUE)
  expect_equal(as_flag(FALSE), FALSE)

  v1 <- 1
  v2 <- letters
  expect_snapshot(error = TRUE, {
    as_flag(v1)
    as_flag(v2)
  })
})

test_that("as_count", {
  expect_null(as_count(NULL, null = TRUE))
  expect_equal(as_count(1), 1L)
  expect_equal(as_count(10L), 10L)
  expect_equal(as_count("100"), 100L)

  v1 <- 1:3
  v2 <- NA_integer_
  v3 <- -1
  v4 <- mtcars
  expect_snapshot(error = TRUE, {
    as_count(v1)
    as_count(v2)
    as_count(v3)
    as_count(v4)
  })
})

test_that("as_choice", {
  expect_equal(as_choice("a", letters), "a")
  expect_equal(as_choice("X", letters), "x")

  v1 <- "z"
  expect_snapshot(error = TRUE, {
    as_choice(v1, letters[1:5])
  })

  v2 <- 1:10
  expect_snapshot(error = TRUE, {
    as_choice(v2, letters[1:5])
  })
})

test_that("default options", {
  expect_snapshot({
    as_tsjson_options(NULL)
  })
})

test_that("as_tsjson_options", {
  expect_snapshot({
    as_tsjson_options(list(format = "compact", allow_comments = FALSE))
  })

  v1 <- 1:10
  v2 <- list("foo" = "bar", "baz")
  v3 <- list(foo = "bar")
  v4 <- list(allow_empty_content = "yes")
  v5 <- list(allow_comments = "nope")
  v6 <- list(format = "fancy")
  v7 <- list(indent_width = -2)
  v8 <- list(indent_style = "fancy")
  expect_snapshot(error = TRUE, {
    as_tsjson_options(v1)
    as_tsjson_options(v2)
    as_tsjson_options(v3)
    as_tsjson_options(v4)
    as_tsjson_options(v5)
    as_tsjson_options(v6)
    as_tsjson_options(v7)
    as_tsjson_options(v8)
  })
})
