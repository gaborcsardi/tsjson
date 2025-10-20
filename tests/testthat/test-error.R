test_that("cnd", {
  do <- function(foo) {
    expect_snapshot(cnd("foo {caller_arg(foo)} bar"))
  }
  do(42)
})

test_that("caller_arg", {
  foo <- 100
  expect_snapshot(caller_arg(foo))
})

test_that("as_caller_arg", {
  expect_snapshot(as_caller_arg(42))
})

test_that("frame_get", {
  # special cases
  skip_on_cran()
  expect_null(frame_get(.GlobalEnv))
  fake(frame_get, "evalq", function(...) list())
  expect_null(frame_get(environment(), sys.frame))
})

test_that("check_named_arg", {
  f <- function(foobar = NULL) {
    if (!missing(foobar)) {
      check_named_arg(foobar)
    }
  }
  expect_silent(f(foobar = 42))
  expect_silent(f())

  expect_snapshot(error = TRUE, {
    f(42)
    f(fooba = 42)
  })
})

test_that("format_tsjson_parse_error_1 wide rows", {
  withr::local_options(list(cli_width = 60))
  # error: value without quotes
  key <- strrep("foo", 30)
  value <- strrep("bar", 30)
  text <- glue('{{"{key}": {value}, "b": 123123123 }}')
  expect_snapshot(error = TRUE, {
    load_json(text = text)
  })
})

test_that("format.tsjson_parse_error", {
  text <- '{"key": value, "b": 123123123 }'
  err <- tryCatch(load_json(text = text), error = function(e) e)
  expect_snapshot({
    format(err)
  })
})

test_that("print.tsjson_parse_error", {
  text <- '{"key": value, "b": 123123123 }'
  err <- tryCatch(load_json(text = text), error = function(e) e)
  expect_snapshot({
    print(err)
  })
})
