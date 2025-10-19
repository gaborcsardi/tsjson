# insert_into_selected

    Code
      insert_into_selected(select(json, "b"), "foo", at = 1)
    Output
      # json (1 line)
      1 | { "a": true, "b": [ 1, "foo", 2, 3 ] }

# insert_into_selected with empty selection

    Code
      insert_into_selected(select(json, "new"), "foo")
    Output
      # json (1 line, 0 selected elements)
      1 | { "a": true, "b": [1, 2, 3] }

# insert_into_selected multi-line array is pretty

    Code
      insert_into_selected(select(json, "b"), list(a = 1, b = 2))
    Output
      # json (9 lines)
      1 | { "a": true, "b": [
      2 |     1,
      3 |     2,
      4 |     3,
      5 |     {
      6 |         "a": 1,
      7 |         "b": 2
      8 |     }
      9 | ] }

# insert_into_selected with compact array is compact

    Code
      insert_into_selected(select(json, "b"), list(1, 2))
    Output
      # json (1 line)
      1 | { "a":true, "b":[1,2,3,[1,2]] }

# insert_into_selected document

    Code
      insert_into_selected(json, list(a = 1, b = 2))
    Output
      # json (4 lines)
      1 | {
      2 |   "a": 1,
      3 |   "b": 2
      4 | }

# insert_into_selected object

    Code
      insert_into_selected(select(json, "a"), 42, key = "b")
    Output
      # json (1 line)
      1 | { "a": { "b": 42 } } 

---

    Code
      insert_into_selected(select(json, "a"), 43, key = "c")
    Output
      # json (1 line)
      1 | { "a": { "b": 42, "c": 43 } } 

# insert_into_selectied force formatting

    Code
      insert_into_selected(select(json, "b"), list(1, 2), format = "pretty")
    Output
      # json (9 lines)
      1 | { "a":true, "b":[
      2 |     1,
      3 |     2,
      4 |     3,
      5 |     [
      6 |         1,
      7 |         2
      8 |     ]
      9 | ] }

# insert_into_document errors

    Code
      insert_into_document(json, "true", "pretty")
    Condition
      Error in `insert_into_document()`:
      ! Cannot insert JSON element at the document root if the document already has other non-comment elements.

# insert_into_selected adds newline if needed

    Code
      insert_into_selected(json, list(a = 1, b = 2))
    Output
      # json (5 lines)
      1 | // comment
      2 | {
      3 |   "a": 1,
      4 |   "b": 2
      5 | }

# insert_into_selected invalid index

    Code
      insert_into_selected(select(json, "b"), "foo", at = "bar")
    Condition
      Error in `insert_into_array()`:
      ! Invalid `at` value for inserting JSON element into array. It must be an integer scalar or `Inf`.

# insert_into_selected insert into empty array

    Code
      insert_into_selected(select(json, "b"), "foo")
    Output
      # json (1 line)
      1 | { "a": true, "b": ["foo"] }

# insert_into_selected insert at beginning of array

    Code
      insert_into_selected(select(json, "b"), "foo", at = 0)
    Output
      # json (1 line)
      1 | { "a": true, "b": ["foo",1] }

# insert_into_selected insert into object by key

    Code
      insert_into_selected(json, "val", key = "key", at = "a")
    Output
      # json (1 line)
      1 | { "a": true, "key": "val", "b": [ 1 ] }

# insert_into_selected insert into object by non-existing key

    Code
      insert_into_selected(json, "val", key = "key", at = "nope")
    Output
      # json (1 line)
      1 | { "a": true, "b": [ 1 ], "key": "val" }

# insert_into_selected insert into object at be beginning

    Code
      insert_into_selected(json, "val", key = "key", at = 0)
    Output
      # json (1 line)
      1 | { "key": "val", "a": true, "b": [ 1 ] }

