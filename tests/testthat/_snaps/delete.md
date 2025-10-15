# delete_selected comment is deleted

    Code
      delete_selected(select(json, "a"))
    Output
      # json (1 line)
      1 | { "b": [1, 2, 3] }

# delete_selected comment is preserved

    Code
      delete_selected(select(json, "a"))
    Output
      # json (3 lines)
      1 | { "b": [1, 2, 3]
      2 | //comment
      3 | }

# delete_selected nothing to delete

    Code
      delete_selected(select(json, "c"))
    Output
      # json (1 line)
      1 | { "a": true, "b": [1, 2, 3] }

# delete_selected all elements from an array

    Code
      delete_selected(select(json, "b", 1))
    Output
      # json (1 line)
      1 | { "a": true, "b": [] }

# delete_selected first elements from an array

    Code
      delete_selected(select(json, "b", 1:2))
    Output
      # json (1 line)
      1 | { "a": true, "b": [3] }

# delete_selected middle of an array

    Code
      delete_selected(select(json, "b", 2:3))
    Output
      # json (1 line)
      1 | { "a": true, "b": [1, 4] }

# delete_selected last elements of an array

    Code
      delete_selected(select(json, "b", 3:4))
    Output
      # json (1 line)
      1 | { "a": true, "b": [1, 2] }

# delete_selected whitespace of last element is kept

    Code
      delete_selected(select(json, "b"))
    Output
      # json (2 lines)
      1 | { "a": true 
      2 |  }

