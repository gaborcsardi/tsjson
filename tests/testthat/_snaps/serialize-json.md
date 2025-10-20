# serialize_json

    Code
      serialize_json(NULL)
    Output
      [1] "null"
    Code
      serialize_json(TRUE)
    Output
      [1] "true"
    Code
      serialize_json(FALSE)
    Output
      [1] "false"
    Code
      serialize_json(1L)
    Output
      [1] "1"
    Code
      serialize_json(1.1)
    Output
      [1] "1.1"
    Code
      serialize_json(0.25)
    Output
      [1] "0.25"

---

    Code
      serialize_json(list())
    Output
      [1] "[]"
    Code
      writeLines(serialize_json(list(1)))
    Output
      [
        1
      ]
    Code
      writeLines(serialize_json(list(1, 2, 3)))
    Output
      [
        1,
        2,
        3
      ]
    Code
      writeLines(serialize_json(list(1, list(21, 22), 3)))
    Output
      [
        1,
        [
          21,
          22
        ],
        3
      ]
    Code
      writeLines(serialize_json(list(1, list(a = 1, b = 2), 3)))
    Output
      [
        1,
        {
          "a": 1,
          "b": 2
        },
        3
      ]

---

    Code
      writeLines(serialize_json(structure(list(), names = character())))
    Output
      {}
    Code
      writeLines(serialize_json(list(a = 1)))
    Output
      {
        "a": 1
      }
    Code
      writeLines(serialize_json(list(a = 1, b = 2, c = 3)))
    Output
      {
        "a": 1,
        "b": 2,
        "c": 3
      }
    Code
      writeLines(serialize_json(list(a = 1, b = list(21, 22), c = 3)))
    Output
      {
        "a": 1,
        "b": [
          21,
          22
        ],
        "c": 3
      }
    Code
      writeLines(serialize_json(list(a = 1, b = list(b1 = 21, b2 = 22), c = 3)))
    Output
      {
        "a": 1,
        "b": {
          "b1": 21,
          "b2": 22
        },
        "c": 3
      }

# serialize_json collapse

    Code
      (txt <- serialize_json(list(a = 1, b = 2), collapse = TRUE))
    Output
      [1] "{\n  \"a\": 1,\n  \"b\": 2\n}"
    Code
      writeLines(txt)
    Output
      {
        "a": 1,
        "b": 2
      }

# serialize_json format

    Code
      writeLines(serialize_json(list(1, 2, 3), options = list(format = "compact")))
    Output
      [1,2,3]
    Code
      writeLines(serialize_json(list(a = 1, b = 2), options = list(format = "compact")))
    Output
      {"a":1,"b":2}

---

    Code
      writeLines(serialize_json(list(1, 2, 3), options = list(format = "oneline")))
    Output
      [ 1, 2, 3 ]
    Code
      writeLines(serialize_json(list(a = 1, b = 2), options = list(format = "oneline")))
    Output
      { "a": 1, "b": 2 }

# serialize_json file

    Code
      writeLines(readLines(tmp))
    Output
      {
        "a": 1,
        "b": {
          "b1": 21,
          "b2": 22
        },
        "c": 3
      }

