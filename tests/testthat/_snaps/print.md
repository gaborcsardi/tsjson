# print.tsjson

    Code
      json
    Output
      # json (10 lines)
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

# format_tsjson_noselection

    Code
      json[["no-such-element"]]
    Output
      # json (10 lines, 0 selected elements)
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

# format_tsjson_noselection long document

    Code
      json
    Output
      # json (30 lines)
       1 | {
       2 |   "a": [
       3 |     "a",
       4 |     "b",
       5 |     "c",
       6 |     "d",
       7 |     "e",
       8 |     "f",
       9 |     "g",
      10 |     "h",
      i 20 more lines
      i Use `print(n = ...)` to see more lines

# format_tsjson_selection

    Code
      json[["a"]]
    Output
      # json (10 lines, 1 selected element)
         1  | {
      >  2  |   "a": [
      >  3  |     1,
      >  4  |     2,
      >  5  |     3
      >  6  |   ],
         7  |   "b": {
         8  |     "b1": "foo"
         9  |   }
        ...   
    Code
      json[["a"]][[1:2]]
    Output
      # json (10 lines, 2 selected elements)
        1   | {
        2   |   "a": [
      > 3   |     1,
      > 4   |     2,
        5   |     3
        6   |   ],
        7   |   "b": {
        ...   
    Code
      json[[list("b", "b1")]]
    Output
      # json (10 lines, 1 selected element)
        ...   
         5  |     3
         6  |   ],
         7  |   "b": {
      >  8  |     "b1": "foo"
         9  |   }
        10  | }

# many selections

    Code
      json[["a"]][[seq(2, 30, by = 2)]]
    Output
      # json (104 lines, 15 selected elements)
         1  | {
         2  |   "a": [
         3  |     1,
      >  4  |     2,
         5  |     3,
      >  6  |     4,
         7  |     5,
      >  8  |     6,
         9  |     7,
      > 10  |     8,
        11  |     9,
      > 12  |     10,
        13  |     11,
      > 14  |     12,
        15  |     13,
      > 16  |     14,
        17  |     15,
      > 18  |     16,
        19  |     17,
      > 20  |     18,
        21  |     19,
      > 22  |     20,
        23  |     21,
        24  |     22,
        25  |     23,
        ...   
      i 5 more selected elements
      i Use `print(n = ...)` to see more selected elements

