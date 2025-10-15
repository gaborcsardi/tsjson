# update_selected

    Code
      update_selected(select(json, "a"), list("new", "element"))
    Output
      # json (4 lines)
      1 | { "a": [
      2 |   "new",
      3 |   "element"
      4 | ], "b": [1, 2, 3] }

# update_selected with empty selection can be an insert

    Code
      upd <- update_selected(select(json, "new", "element"), list("new", "value"))
      print(upd, n = Inf)
    Output
      # json (1 line)
      1 | { "a": true, "b": [ 1, 2, 3 ], "new": { "element": [ "new", "value" ] } }

# updated_selected with empry non-character selection is noop

    Code
      upd <- update_selected(select(json, "b", 10), list("new", "value"))
      print(upd, n = Inf)
    Output
      # json (1 line, 0 selected elements)
      1 | { "a": true, "b": [1, 2, 3] }

