# can modify objects by name

    Code
      update_selected(select(load_json(text = "{}"), "foo"), 1)
    Output
      # json (3 lines)
      1 | {
      2 |     "foo": 1
      3 | }

---

    Code
      update_selected(select(load_json(text = "{}"), "foo"), 1:2)
    Output
      # json (6 lines)
      1 | {
      2 |     "foo": [
      3 |         1,
      4 |         2
      5 |     ]
      6 | }

---

    Code
      update_selected(select(load_json(text = "{}"), "foo"), list(1, "x"))
    Output
      # json (6 lines)
      1 | {
      2 |     "foo": [
      3 |         1,
      4 |         "x"
      5 |     ]
      6 | }

# modification retains comments

    Code
      update_selected(select(load_json(text = text), "foo"), 0)
    Output
      # json (14 lines)
       1 | 
       2 | {
       3 |     // a
       4 |     "foo": 0, // b
       5 |     "bar": [
       6 |         // c
       7 |         1,
       8 |         2, // d
       9 |         // e
      10 |         3
      i 4 more lines
      i Use `print(n = ...)` to see more lines

---

    Code
      update_selected(select(load_json(text = text), "bar", 2), 0)
    Output
      # json (14 lines)
       1 | 
       2 | {
       3 |     // a
       4 |     "foo": 1, // b
       5 |     "bar": [
       6 |         // c
       7 |         1,
       8 |         0, // d
       9 |         // e
      10 |         3
      i 4 more lines
      i Use `print(n = ...)` to see more lines

---

    Code
      print(insert_into_selected(select(load_json(text = text), "bar"), 0, at = 2),
      n = 20)
    Output
      # json (15 lines)
       1 | 
       2 | {
       3 |     // a
       4 |     "foo": 1, // b
       5 |     "bar": [
       6 |         // c
       7 |         1,
       8 |         2, // d
       9 |         0,
      10 |         // e
      11 |         3
      12 |     ] // f
      13 |     // g
      14 | }
      15 |   

