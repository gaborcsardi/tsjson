# ts_tree_selection, ts_tree_selected_nodes

    Code
      ts_tree_selection(json)
    Output
      [[1]]
      [[1]]$selector
      list()
      attr(,"class")
      [1] "ts_tree_selector_default" "ts_tree_selector"        
      [3] "list"                    
      
      [[1]]$nodes
      [1] 1
      
      
    Code
      ts_tree_selection(json, default = FALSE)
    Output
      NULL
    Code
      ts_tree_selected_nodes(json)
    Output
      [1] 1
    Code
      ts_tree_selected_nodes(json, default = FALSE)
    Output
      integer(0)

---

    Code
      ts_tree_selection(json)
    Output
      [[1]]
      [[1]]$selector
      list()
      attr(,"class")
      [1] "ts_tree_selector_default" "ts_tree_selector"        
      [3] "list"                    
      
      [[1]]$nodes
      [1] 2
      
      
    Code
      ts_tree_selection(json, default = FALSE)
    Output
      NULL
    Code
      ts_tree_selected_nodes(json)
    Output
      [1] 2
    Code
      ts_tree_selected_nodes(json, default = FALSE)
    Output
      integer(0)

# ts_tree_select

    Code
      ts_tree_select(json)
    Output
      # jsonc (13 lines, 1 selected element)
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
      ts_tree_select(json, "a")
    Output
      # jsonc (13 lines, 1 selected element)
        1   | {
      > 2   |   "a": 1,
        3   |   "b": {
        4   |     "b1": 21,
        5   |     "b2": 22
        ...   
    Code
      ts_tree_select(json, c("a", "b"))
    Output
      # jsonc (13 lines, 2 selected elements)
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
      ts_tree_select(json, "b", "b1")
    Output
      # jsonc (13 lines, 1 selected element)
        1   | {
        2   |   "a": 1,
        3   |   "b": {
      > 4   |     "b1": 21,
        5   |     "b2": 22
        6   |   },
        7   |   "c": 3,
        ...   
    Code
      ts_tree_select(json, list("b", "b1"))
    Output
      # jsonc (13 lines, 1 selected element)
        1   | {
        2   |   "a": 1,
        3   |   "b": {
      > 4   |     "b1": 21,
        5   |     "b2": 22
        6   |   },
        7   |   "c": 3,
        ...   
    Code
      ts_tree_select(json, "d", 1)
    Output
      # jsonc (13 lines, 1 selected element)
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
      ts_tree_select(json, "d", TRUE)
    Output
      # jsonc (13 lines, 3 selected elements)
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
      ts_tree_select(json, raw(0))
    Condition
      Error in `ts_tree_select1.default()`:
      ! Don't know how to select nodes from a `ts_tree` (JSONC) object using selector of class `raw`.

# deselect with NULL

    Code
      ts_tree_select(json, "a")
    Output
      # jsonc (4 lines, 1 selected element)
        1 | {
      > 2 |   "a": 1,
        3 |   "c": 3
        4 | }
    Code
      ts_tree_select(ts_tree_select(json, "a"), NULL)
    Output
      # jsonc (4 lines)
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

# [[<-.tsjsonc

    Code
      json[["a"]] <- 2
      json
    Output
      # jsonc (13 lines)
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
      # jsonc (13 lines)
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
      # jsonc (13 lines)
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
      # jsonc (13 lines)
       1 | {
       2 |   "a": 1,
       3 |   "b": {
       4 |     "b1": 100,
       5 |     "b2": 22
       6 |   },
       7 |   "c": 3,
       8 |   "d": [
       9 |     1,
      10 |     2,
      i 3 more lines
      i Use `print(n = ...)` to see more lines

# [[<-.tsjsonc empty doc

    Code
      json
    Output
      # jsonc (1 line)
      1 | []

---

    Code
      json
    Output
      # jsonc (1 line)
      1 | {}

# [[<-.tsjsonc deletion

    Code
      json[[c("a", "b")]] <- ts_tree_deleted()
      json
    Output
      # jsonc (8 lines)
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
      list()

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
      

# select I()

    Code
      json[[I(26)]]
    Output
      [[1]]
      [1] 21
      

# ts_tree_select<-

    Code
      ts_tree_select(json, "a") <- 2
      json
    Output
      # jsonc (13 lines)
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
      ts_tree_select(json, c("a")) <- ts_tree_deleted()
      json
    Output
      # jsonc (12 lines)
       1 | {
       2 |   "b": {
       3 |     "b1": 21,
       4 |     "b2": 22
       5 |   },
       6 |   "c": 3,
       7 |   "d": [
       8 |     1,
       9 |     2,
      10 |     3
      i 2 more lines
      i Use `print(n = ...)` to see more lines

---

    Code
      ts_tree_select(json, list("b", "b1")) <- 100
      json
    Output
      # jsonc (12 lines)
       1 | {
       2 |   "b": {
       3 |     "b1": 100,
       4 |     "b2": 22
       5 |   },
       6 |   "c": 3,
       7 |   "d": [
       8 |     1,
       9 |     2,
      10 |     3
      i 2 more lines
      i Use `print(n = ...)` to see more lines

# select character selector on array selects nothing

    Code
      json[["d", "1"]]
    Output
      list()
    Code
      json[["d", "a"]]
    Output
      list()

# select zero indices error

    Code
      json[[list("b", c(1, 2, 0, 3))]]
    Condition
      Error in `ts_tree_select1.ts_tree.integer()`:
      ! Zero indices are not allowed in tsitter selectors.

# TS query

    Code
      ts_tree_update(ts_tree_select(ts_parse_jsonc(text = txt), query = "((pair value: (number) @num))"),
      100)
    Output
      # jsonc (1 line)
      1 | { "a": 100, "b": "foo", "c": 100}

