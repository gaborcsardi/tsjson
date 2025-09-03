# save_json

    Code
      load_json(tmp)
    Output
      # json (test.json, 10 lines)
       1 | {
       2 |   "a": [
       3 |     1,
       4 |     2,
       5 |     3
       6 |   ],
       7 |   "b": {
       8 |     "b1": "foo"
       9 |   }
      10 | }

---

    Code
      save_json(json)
    Condition
      Error in `save_json()`:
      ! Don't know which file to save JSON document to. You need to specify the `file` argument.

---

    Code
      load_json(tmp)
    Output
      # json (test.json, 5 lines)
      1 | {
      2 |   "b": {
      3 |     "b1": "foo"
      4 |   }
      5 | }

---

    Code
      save_json(json, file = stdout())
    Output
      {
        "a": [
          1,
          2,
          3
        ],
        "b": {
          "b1": "foo"
        }
      }

---

    Code
      load_json(tmp2)
    Output
      # json (bin.json, 10 lines)
       1 | {
       2 |   "a": [
       3 |     1,
       4 |     2,
       5 |     3
       6 |   ],
       7 |   "b": {
       8 |     "b1": "foo"
       9 |   }
      10 | }

