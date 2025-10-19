test_that("is_flag", {
  expect_true(is_flag(TRUE))
  expect_true(is_flag(FALSE))
  expect_false(is_flag(1))
  expect_false(is_flag("TRUE"))
  expect_false(is_flag(c(TRUE, FALSE)))
})
