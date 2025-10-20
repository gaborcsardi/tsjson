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

