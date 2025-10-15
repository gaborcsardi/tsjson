# format_json

    Code
      writeLines(format_json(text = text))
    Output
      {
        "a": 1,
        "b": {
          "b1": 21,
          "b2": 22
        },
        "c": 3,
        "d": [
          1,
          2,
          3
        ]
      }

# format_selected

    Code
      format_selected(json)
    Output
      # json (13 lines)
       1 | {
       2 |   "a": 1,
       3 |   "b": {
       4 |     "b1": 21,
       5 |     "b2": 22
       6 |   },
       7 |   "c": 3,
       8 |   "d": [
       9 |     1,
      10 |     2,
      i 3 more lines
      i Use `print(n = ...)` to see more lines
    Code
      format_selected(select(json, "a"))
    Output
      # json (1 line)
      1 | {"a":1,"b":{"b1":21,"b2":22},"c":3,"d":[1,2,3]}
    Code
      format_selected(select(json, "b"))
    Output
      # json (4 lines)
      1 | {"a":1,"b":{
      2 |   "b1": 21,
      3 |   "b2": 22
      4 | },"c":3,"d":[1,2,3]}

---

    Code
      format_selected(select(json, "b"))
    Output
      # json (13 lines)
       1 | {
       2 |   "a": 1,
       3 |   "b": {
       4 |     "b1": 21,
       5 |     "b2": 22
       6 |   },
       7 |   "c": 3,
       8 |   "d": [
       9 |     1,
      10 |     2,
      i 3 more lines
      i Use `print(n = ...)` to see more lines

# format_selected null, true, false, string, comment

    Code
      format_selected(json)
    Output
      # json (10 lines)
       1 | {
       2 |   "a": null,
       3 |   "b": true,
       4 |   "c": false,
       5 |   "d": [
       6 |     "a",
       7 |     "b",
       8 |     "c"
       9 |   ]
      10 | }

---

    Code
      format_selected(json)
    Output
      # json (6 lines)
      1 | {
      2 |   // comment
      3 |   "a":
      4 |     // comment
      5 |     null
      6 | }

# format_selected empty array

    Code
      format_selected(json)
    Output
      # json (4 lines)
      1 | {
      2 |   "a": [],
      3 |   "b": true
      4 | }

# format_selected compact arrays

    Code
      json
    Output
      # json (8 lines)
      1 | {
      2 |   "a": [
      3 |     1,
      4 |     2,
      5 |     3
      6 |   ],
      7 |   "b": true
      8 | }
    Code
      format_selected(json, "compact")
    Output
      # json (1 line)
      1 | {"a":[1,2,3],"b":true}

# format_selected oneline arrays

    Code
      json
    Output
      # json (8 lines)
      1 | {
      2 |   "a": [
      3 |     1,
      4 |     2,
      5 |     3
      6 |   ],
      7 |   "b": true
      8 | }
    Code
      format_selected(json, "oneline")
    Output
      # json (1 line)
      1 | { "a": [ 1, 2, 3 ], "b": true }

# format_selected empty object

    Code
      format_selected(json)
    Output
      # json (4 lines)
      1 | {
      2 |   "a": {},
      3 |   "b": true
      4 | }

# format_selected drop comments in compact, oneline modes

    Code
      json
    Output
      # json (4 lines)
      1 | { // comment
      2 |   "a": // comment
      3 |     null
      4 | }
    Code
      format_selected(json, "compact")
    Output
      # json (1 line)
      1 | {"a":null}
    Code
      format_selected(json, "oneline")
    Output
      # json (1 line)
      1 | { "a": null }

