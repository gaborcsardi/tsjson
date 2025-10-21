# error messaging is reasonably helpful

    Code
      unserialize_json(text = text)
    Condition
      Error in `token_table()`:
      ! JSON parse error `<text>`:2:3
      1| {
      2|   "a" 1
           ^^^^^
      3| }

---

    Code
      unserialize_json(text = text)
    Condition
      Error in `token_table()`:
      ! JSON parse error `<text>`:2:3
      1| {
      2|   "a": ]
           ^^^^^^
      3| }

---

    Code
      unserialize_json(text = text)
    Condition
      Error in `token_table()`:
      ! JSON parse error `<text>`:5:5
      4|     2,
      5|     b"
             ^^
      6|   ]

---

    Code
      unserialize_json(text = text)
    Condition
      Error in `token_table()`:
      ! JSON parse error `<text>`:3:5
      2|   "a": [
      3|     b",
             ^^^
      4|     2,

# `allow_comments` works

    Code
      unserialize_json(text = text, options = list(allow_comments = FALSE))
    Condition
      Error in `token_table()`:
      ! The JSON document contains comments, and this is not allowed. To allow comments, set the `allow_comments` option to `TRUE`.

# `allow_trailing_comma` works

    Code
      unserialize_json(text = text, options = list(allow_trailing_comma = FALSE))
    Condition
      Error in `token_table()`:
      ! The JSON document contains trailing commas, and this is not allowed. To allow trailing commas, set the `allow_trailing_comma` option to `TRUE`.

# `allow_empty_content` works

    Code
      unserialize_json(text = "", options = options)
    Condition
      Error in `token_table()`:
      ! The JSON document is empty, and this is not allowed. To allow this, set the `allow_empty_content` option to `TRUE`.

