test_that("empty file parsing works", {
  expect_identical(unserialize_json(text = ""), NULL)
  expect_identical(
    load_json(text = "") |> select("a") |> unserialize_selected(),
    list(NULL)
  )
})

test_that("Output is always returned visibly", {
  expect_identical(withVisible(unserialize_json(text = "{}"))$visible, TRUE)
  expect_identical(
    withVisible(
      load_json(text = '{ "a": 1 }') |> select("a") |> unserialize_selected()
    )$visible,
    TRUE
  )

  # These must return visible `NULL`
  expect_identical(withVisible(unserialize_json(text = ""))$visible, TRUE)
  expect_identical(
    withVisible(
      load_json(text = '{ "a": 1 }') |> select("a") |> unserialize_selected()
    )$visible,
    TRUE
  )

  # These must return visible `NULL`
  expect_identical(withVisible(unserialize_json(text = "null"))$visible, TRUE)
  expect_identical(
    withVisible(
      load_json(text = '{ "a": null }') |> select("a") |> unserialize_selected()
    )$visible,
    TRUE
  )
})

test_that("works outside of a base object `{`", {
  expect_identical(unserialize_json(text = "1"), 1L)
  expect_identical(unserialize_json(text = "1.5"), 1.5)
  expect_identical(unserialize_json(text = "true"), TRUE)
  expect_identical(unserialize_json(text = "[1,2]"), list(1L, 2L))
  expect_identical(unserialize_json(text = "[1,true]"), list(1L, TRUE))
})

test_that("`null` converts correctly", {
  expect_identical(unserialize_json(text = "null"), NULL)
  expect_identical(unserialize_json(text = '[null]'), list(NULL))
  expect_identical(unserialize_json(text = '[null, null]'), list(NULL, NULL))
  expect_identical(
    unserialize_json(text = '[null, 1, null]'),
    list(NULL, 1L, NULL)
  )
  expect_identical(unserialize_json(text = '{"a": null}'), list(a = NULL))
})

test_that("works with objects", {
  text <- '
  {
    "a": 1,
    "b": {
      "c": 2
    }
  }
  '
  expect_identical(
    unserialize_json(text = text),
    list(a = 1L, b = list(c = 2L))
  )
})

test_that("works with array as property value", {
  # Homogenous
  text <- '
  {
    "a": [1, 2]
  }
  '
  expect_identical(
    unserialize_json(text = text),
    list(a = list(1L, 2L))
  )

  # Heterogeneous
  text <- '
  {
    "a": [1, "2"]
  }
  '
  expect_identical(
    unserialize_json(text = text),
    list(a = list(1L, "2"))
  )
})

test_that("works with array of array as property value", {
  text <- '
  {
    "a": [[1], [2]]
  }
  '
  expect_identical(
    unserialize_json(text = text),
    list(a = list(list(1L), list(2L)))
  )

  text <- '
  {
    "a": [[1,2], [3,4]]
  }
  '
  expect_identical(
    unserialize_json(text = text),
    list(a = list(list(1L, 2L), list(3L, 4L)))
  )

  # Mixed types are allowed in JSON!
  # This is one big reason we don't simplify!
  text <- '
  {
    "a": [[1,"2"], [true,4]]
  }
  '
  expect_identical(
    unserialize_json(text = text),
    list(a = list(list(1L, "2"), list(TRUE, 4L)))
  )

  text <- '
  {
    "a": [["a"], [1]]
  }
  '
  expect_identical(
    unserialize_json(text = text),
    list(a = list(list("a"), list(1L)))
  )

  # This is another reason we don't try and simplify.
  # If the lengths were the same jsonlite simplifies to a matrix,
  # but if they aren't we'd get a list. That's hard to program around.
  text <- '
  {
    "a": [[1], [2, 3]]
  }
  '
  expect_identical(
    unserialize_json(text = text),
    list(a = list(list(1L), list(2L, 3L)))
  )
})

test_that("error messaging is reasonably helpful", {
  text <- trimws(
    '
{
  "a" 1
}
    '
  )
  expect_snapshot(error = TRUE, unserialize_json(text = text))

  text <- trimws(
    '
{
  "a": ]
}
    '
  )
  expect_snapshot(error = TRUE, unserialize_json(text = text))

  text <- trimws(
    '
{
  "a": [
    1,
    2,
    b"
  ]
}
    '
  )
  expect_snapshot(error = TRUE, unserialize_json(text = text))

  text <- trimws(
    '
{
  "a": [
    b",
    2,
    3
  ]
}
    '
  )
  expect_snapshot(error = TRUE, unserialize_json(text = text))
})

test_that("`allow_comments` works", {
  text <- '
  {
    // A comment!
    "a": 1
  }
  '

  expect_snapshot(error = TRUE, {
    unserialize_json(text = text, options = list(allow_comments = FALSE))
  })
})

test_that("`allow_trailing_comma` works", {
  text <- '
  {
    "a": 1,
  }
  '

  expect_snapshot(error = TRUE, {
    unserialize_json(text = text, options = list(allow_trailing_comma = FALSE))
  })
})

test_that("`allow_empty_content` works", {
  options <- list(allow_empty_content = FALSE)

  expect_identical(
    unserialize_json(text = '"a"', options = options),
    "a"
  )

  expect_snapshot(error = TRUE, {
    unserialize_json(text = "", options = options)
  })
})
