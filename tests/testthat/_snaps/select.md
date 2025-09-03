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
      select(json, "d", TRUE)
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
      [[1]]
      [[1]]$a
      [1] 1
      
      [[1]]$b
      [[1]]$b$b1
      [1] 21
      
      [[1]]$b$b2
      [1] 22
      
      
      [[1]]$c
      [1] 3
      
      [[1]]$d
      [[1]]$d[[1]]
      [1] 1
      
      [[1]]$d[[2]]
      [1] 2
      
      [[1]]$d[[3]]
      [1] 3
      
      
      
    Code
      json[["a"]]
    Output
      [[1]]
      [1] 1
      
    Code
      json[[c("a", "b")]]
    Output
      [[1]]
      [1] 1
      
      [[2]]
      [[2]]$b1
      [1] 21
      
      [[2]]$b2
      [1] 22
      
      
    Code
      json[["b", "b1"]]
    Output
      [[1]]
      [1] 21
      
    Code
      json[[list("b", "b1")]]
    Output
      [[1]]
      [1] 21
      
    Code
      json[["d", 1]]
    Output
      [[1]]
      [1] 1
      
    Code
      json[["d", TRUE]]
    Output
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2
      
      [[3]]
      [1] 3
      

---

    Code
      json[["d"]][["nothing"]]
    Output
      NULL

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

# select regex

    Code
      json[[c(regex = "^a")]]
    Output
      [[1]]
      [1] 1
      
      [[2]]
      [[2]]$b1
      [1] 21
      
      [[2]]$b2
      [1] 22
      
      
      [[3]]
      [[3]][[1]]
      [1] 1
      
      [[3]][[2]]
      [1] 2
      
      [[3]][[3]]
      [1] 3
      
      

---

    Code
      json2[[c(regex = ".")]]
    Output
      list()

# select from the back

    Code
      json[[-1, -2]]
    Output
      [[1]]
      [1] 2
      
    Code
      json[[-1, c(1, -2)]]
    Output
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2
      

# sel_ids

    Code
      json[[sel_ids(26)]]
    Output
      [[1]]
      [1] 21
      

