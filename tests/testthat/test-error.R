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
