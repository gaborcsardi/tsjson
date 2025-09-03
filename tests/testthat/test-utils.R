test_that("%||%", {
  expect_equal(NULL %||% "foo", "foo")
  expect_equal("foo" %||% stop("nope"), "foo")
})

test_that("map_int", {
  expect_equal(
    map_int(1:3, function(x) x),
    1:3
  )

  expect_equal(
    map_int(c(a = 1, b = 2), function(x) as.integer(x)),
    c(a = 1L, b = 2L)
  )

  expect_equal(
    map_int(integer(), function(x) stop("not called")),
    integer()
  )

  expect_equal(
    map_int(letters[1:3], function(x) 1L),
    c(a = 1L, b = 1L, c = 1L)
  )
})

test_that("map_chr", {
  expect_equal(
    map_chr(1:3, function(x) as.character(x)),
    c("1", "2", "3")
  )

  expect_equal(
    map_chr(c(a = 1, b = 2), function(x) as.character(x)),
    c(a = "1", b = "2")
  )

  expect_equal(
    map_chr(integer(), function(x) stop("not called")),
    character()
  )

  expect_equal(
    map_chr(letters[1:3], function(x) "x"),
    c(a = "x", b = "x", c = "x")
  )
})

test_that("na_omit", {
  expect_equal(na_omit(letters), letters)
  expect_equal(na_omit(character()), character())
  expect_equal(na_omit(integer()), integer())

  expect_equal(na_omit(NA_integer_), integer())
  expect_equal(na_omit(c(1L, NA_integer_, 2L)), 1:2)

  expect_equal(na_omit(c(1L, NA_integer_, 2L, NA_integer_)), 1:2)
})

test_that("middle", {
  expect_equal(middle(integer()), integer())
  expect_equal(middle(1L), integer())
  expect_equal(middle(1:2), integer())
  expect_equal(middle(1:10), 2:9)
})

test_that("max_or_na", {
  expect_equal(max_or_na(integer()), NA_integer_)
  expect_equal(max_or_na(double()), NA_real_)
  expect_equal(max_or_na(1:10), 10L)
})
