# get_selection, get_selected_nodes

    Code
      get_selection(json)
    Output
      [[1]]
      [[1]]$selector
      list()
      attr(,"class")
      [1] "tsjson_selector_default" "tsjson_selector"        
      [3] "list"                   
      
      [[1]]$nodes
      [1] 1
      
      
    Code
      get_selection(json, default = FALSE)
    Output
      NULL
    Code
      get_selected_nodes(json)
    Output
      [1] 1
    Code
      get_selected_nodes(json, default = FALSE)
    Output
      integer(0)

---

    Code
      get_selection(json)
    Output
      [[1]]
      [[1]]$selector
      list()
      attr(,"class")
      [1] "tsjson_selector_default" "tsjson_selector"        
      [3] "list"                   
      
      [[1]]$nodes
      [1] 2
      
      
    Code
      get_selection(json, default = FALSE)
    Output
      NULL
    Code
      get_selected_nodes(json)
    Output
      [1] 2
    Code
      get_selected_nodes(json, default = FALSE)
    Output
      integer(0)

---

    Code
      json
    Output
      # json (1 line)
      1 | [1,2,3]
    Code
      json[[sel_all()]]
    Output
      # json (1 line, 3 selected elements)
      > 1 | [1,2,3]
    Code
      get_selection(json[[sel_all()]])
    Output
      [[1]]
      [[1]]$selector
      list()
      attr(,"class")
      [1] "tsjson_selector_default" "tsjson_selector"        
      [3] "list"                   
      
      [[1]]$nodes
      [1] 2
      
      
      [[2]]
      [[2]]$selector
      list()
      attr(,"class")
      [1] "tsjson_selector_all" "tsjson_selector"     "list"               
      
      [[2]]$nodes
      [1] 4 6 8
      
      
    Code
      get_selected_nodes(json[[sel_all()]])
    Output
      [1] 4 6 8

# select

    Code
      select(json)
    Output
      # json (13 lines, 1 selected element)
      >  1 | {
      >  2 |   "a": 1,
      >  3 |   "b": {
      >  4 |     "b1": 21,
      >  5 |     "b2": 22
      >  6 |   },
      >  7 |   "c": 3,
      >  8 |   "d": [
      >  9 |     1,
      > 10 |     2,
      > 11 |     3
      > 12 |   ]
      > 13 | }
    Code
      select(json, "a")
    Output
      # json (13 lines, 1 selected element)
        1   | {
      > 2   |   "a": 1,
        3   |   "b": {
        4   |     "b1": 21,
        5   |     "b2": 22
        ...   
    Code
      select(json, c("a", "b"))
    Output
      # json (13 lines, 2 selected elements)
         1  | {
      >  2  |   "a": 1,
      >  3  |   "b": {
      >  4  |     "b1": 21,
      >  5  |     "b2": 22
      >  6  |   },
         7  |   "c": 3,
         8  |   "d": [
         9  |     1,
        ...   
    Code
      select(json, "b", "b1")
    Output
      # json (13 lines, 1 selected element)
        1   | {
        2   |   "a": 1,
        3   |   "b": {
      > 4   |     "b1": 21,
        5   |     "b2": 22
        6   |   },
        7   |   "c": 3,
        ...   
    Code
      select(json, list("b", "b1"))
    Output
      # json (13 lines, 1 selected element)
        1   | {
        2   |   "a": 1,
        3   |   "b": {
      > 4   |     "b1": 21,
        5   |     "b2": 22
        6   |   },
        7   |   "c": 3,
        ...   
    Code
      select(json, "d", 1)
    Output
      # json (13 lines, 1 selected element)
        ...   
         6  |   },
         7  |   "c": 3,
         8  |   "d": [
      >  9  |     1,
        10  |     2,
        11  |     3
        12  |   ]
        ...   
    Code
      select(json, "d", sel_all())
    Output
      # json (13 lines, 3 selected elements)
        ...   
         6  |   },
         7  |   "c": 3,
         8  |   "d": [
      >  9  |     1,
      > 10  |     2,
      > 11  |     3
        12  |   ]
        13  | }

---

    Code
      select(json, raw(0))
    Condition
      Error in `select1()`:
      ! Invalid JSON selector

# deselect

    Code
      select(json, "a")
    Output
      # json (4 lines, 1 selected element)
        1 | {
      > 2 |   "a": 1,
        3 |   "c": 3
        4 | }
    Code
      deselect(select(json, "a"))
    Output
      # json (4 lines)
      1 | {
      2 |   "a": 1,
      3 |   "c": 3
      4 | }

# [[.tdjson

    Code
      json[[]]
    Output
      # json (13 lines, 1 selected element)
      >  1 | {
      >  2 |   "a": 1,
      >  3 |   "b": {
      >  4 |     "b1": 21,
      >  5 |     "b2": 22
      >  6 |   },
      >  7 |   "c": 3,
      >  8 |   "d": [
      >  9 |     1,
      > 10 |     2,
      > 11 |     3
      > 12 |   ]
      > 13 | }
    Code
      json[["a"]]
    Output
      # json (13 lines, 1 selected element)
        1   | {
      > 2   |   "a": 1,
        3   |   "b": {
        4   |     "b1": 21,
        5   |     "b2": 22
        ...   
    Code
      json[[c("a", "b")]]
    Output
      # json (13 lines, 2 selected elements)
         1  | {
      >  2  |   "a": 1,
      >  3  |   "b": {
      >  4  |     "b1": 21,
      >  5  |     "b2": 22
      >  6  |   },
         7  |   "c": 3,
         8  |   "d": [
         9  |     1,
        ...   
    Code
      json[["b"]][["b1"]]
    Output
      # json (13 lines, 1 selected element)
        1   | {
        2   |   "a": 1,
        3   |   "b": {
      > 4   |     "b1": 21,
        5   |     "b2": 22
        6   |   },
        7   |   "c": 3,
        ...   
    Code
      json[[list("b", "b1")]]
    Output
      # json (13 lines, 1 selected element)
        1   | {
        2   |   "a": 1,
        3   |   "b": {
      > 4   |     "b1": 21,
        5   |     "b2": 22
        6   |   },
        7   |   "c": 3,
        ...   
    Code
      json[["d"]][[1]]
    Output
      # json (13 lines, 1 selected element)
        ...   
         6  |   },
         7  |   "c": 3,
         8  |   "d": [
      >  9  |     1,
        10  |     2,
        11  |     3
        12  |   ]
        ...   
    Code
      json[["d"]][[sel_all()]]
    Output
      # json (13 lines, 3 selected elements)
        ...   
         6  |   },
         7  |   "c": 3,
         8  |   "d": [
      >  9  |     1,
      > 10  |     2,
      > 11  |     3
        12  |   ]
        13  | }

---

    Code
      json[["d"]][["nothing"]]
    Output
      # json (13 lines, 0 selected elements)
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

# [[<-.tsjson

    Code
      json[["a"]] <- 2
      json
    Output
      # json (13 lines)
       1 | {
       2 |   "a": 2,
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

---

    Code
      json[[c("a", "c")]] <- TRUE
      json
    Output
      # json (13 lines)
       1 | {
       2 |   "a": true,
       3 |   "b": {
       4 |     "b1": 21,
       5 |     "b2": 22
       6 |   },
       7 |   "c": true,
       8 |   "d": [
       9 |     1,
      10 |     2,
      i 3 more lines
      i Use `print(n = ...)` to see more lines

---

    Code
      json[["b"]][["b1"]] <- 100
      json
    Output
      # json (13 lines)
       1 | {
       2 |   "a": true,
       3 |   "b": {
       4 |     "b1": 100,
       5 |     "b2": 22
       6 |   },
       7 |   "c": true,
       8 |   "d": [
       9 |     1,
      10 |     2,
      i 3 more lines
      i Use `print(n = ...)` to see more lines

---

    Code
      json[[list("b", "b1")]] <- 100
      json
    Output
      # json (13 lines)
       1 | {
       2 |   "a": true,
       3 |   "b": {
       4 |     "b1": 100,
       5 |     "b2": 22
       6 |   },
       7 |   "c": true,
       8 |   "d": [
       9 |     1,
      10 |     2,
      i 3 more lines
      i Use `print(n = ...)` to see more lines

# [[<-.tsjson empty doc

    Code
      json
    Output
      # json (1 line)
      1 | []

---

    Code
      json
    Output
      # json (1 line)
      1 | {}

# [[<-.tsjson deletion

    Code
      json[[c("a", "b")]] <- deleted()
      json
    Output
      # json (8 lines)
      1 | {
      2 |   "c": 3,
      3 |   "d": [
      4 |     1,
      5 |     2,
      6 |     3
      7 |   ]
      8 | }

# sel_regex

    Code
      json[[sel_regex("^a")]]
    Output
      # json (13 lines, 3 selected elements)
         1 | {
      >  2 |   "a1": 1,
      >  3 |   "a2": {
      >  4 |     "b1": 21,
      >  5 |     "b2": 22
      >  6 |   },
         7 |   "c": 3,
      >  8 |   "a3": [
      >  9 |     1,
      > 10 |     2,
      > 11 |     3
      > 12 |   ]
        13 | }

---

    Code
      json2[[sel_regex(".")]]
    Output
      # json (5 lines, 0 selected elements)
      1 | [
      2 |   1,
      3 |   2,
      4 |   3
      5 | ]

# sel_back

    Code
      json[[sel_back(1)]][[sel_back(2)]]
    Output
      # json (13 lines, 1 selected element)
        ...   
         7  |   "c": 3,
         8  |   "d": [
         9  |     1,
      > 10  |     2,
        11  |     3
        12  |   ]
        13  | }

# sel_ids

    Code
      json[[sel_ids(26)]]
    Output
      # json (13 lines, 1 selected element)
        1   | {
        2   |   "a": 1,
        3   |   "b": {
      > 4   |     "b1": 21,
        5   |     "b2": 22
        6   |   },
        7   |   "c": 3,
        ...   

