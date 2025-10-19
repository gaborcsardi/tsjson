# load_json

    Code
      json
    Output
      # json (0 lines)

---

    Code
      json
    Output
      # json (1 line)
      1 | []

---

    Code
      json
    Output
      # json (2 lines)
      1 | // comment
      2 | [1,2,3]
    Code
      json[]
    Output
      # A data frame: 10 x 16
            id parent field_name type     code       start_byte end_byte start_row start_column end_row end_column is_missing has_error expected children  tws  
         <int>  <int> <chr>      <chr>    <chr>           <int>    <int>     <int>        <int>   <int>      <int> <lgl>      <lgl>     <list>   <I<list>> <chr>
       1     1     NA <NA>       document <NA>                0       18         0            0       1          7 FALSE      FALSE     <NULL>   <int [2]> ""   
       2     2      1 <NA>       comment  // comment          0       10         0            0       0         10 FALSE      FALSE     <NULL>   <int [0]> "\n" 
       3     3      1 <NA>       array    <NA>               11       18         1            0       1          7 FALSE      FALSE     <NULL>   <int [7]> ""   
       4     4      3 <NA>       [        [                  11       12         1            0       1          1 FALSE      FALSE     <NULL>   <int [0]> ""   
       5     5      3 <NA>       number   1                  12       13         1            1       1          2 FALSE      FALSE     <NULL>   <int [0]> ""   
       6     6      3 <NA>       ,        ,                  13       14         1            2       1          3 FALSE      FALSE     <NULL>   <int [0]> ""   
       7     7      3 <NA>       number   2                  14       15         1            3       1          4 FALSE      FALSE     <NULL>   <int [0]> ""   
       8     8      3 <NA>       ,        ,                  15       16         1            4       1          5 FALSE      FALSE     <NULL>   <int [0]> ""   
       9     9      3 <NA>       number   3                  16       17         1            5       1          6 FALSE      FALSE     <NULL>   <int [0]> ""   
      10    10      3 <NA>       ]        ]                  17       18         1            6       1          7 FALSE      FALSE     <NULL>   <int [0]> ""   

---

    Code
      json
    Output
      # json (three.json, 2 lines)
      1 | // comment
      2 | [1,2,3]

---

    Code
      json
    Output
      # json (3 lines)
      1 | 
      2 | 
      3 |    [1,2,3]
    Code
      json[]
    Output
      # A data frame: 9 x 16
           id parent field_name type     code  start_byte end_byte start_row start_column end_row end_column is_missing has_error expected children  tws      
        <int>  <int> <chr>      <chr>    <chr>      <int>    <int>     <int>        <int>   <int>      <int> <lgl>      <lgl>     <list>   <I<list>> <chr>    
      1     1     NA <NA>       document <NA>           5       13         2            3       3          0 FALSE      FALSE     <NULL>   <int [1]> "\n\n   "
      2     2      1 <NA>       array    <NA>           5       12         2            3       2         10 FALSE      FALSE     <NULL>   <int [7]> ""       
      3     3      2 <NA>       [        [              5        6         2            3       2          4 FALSE      FALSE     <NULL>   <int [0]> ""       
      4     4      2 <NA>       number   1              6        7         2            4       2          5 FALSE      FALSE     <NULL>   <int [0]> ""       
      5     5      2 <NA>       ,        ,              7        8         2            5       2          6 FALSE      FALSE     <NULL>   <int [0]> ""       
      6     6      2 <NA>       number   2              8        9         2            6       2          7 FALSE      FALSE     <NULL>   <int [0]> ""       
      7     7      2 <NA>       ,        ,              9       10         2            7       2          8 FALSE      FALSE     <NULL>   <int [0]> ""       
      8     8      2 <NA>       number   3             10       11         2            8       2          9 FALSE      FALSE     <NULL>   <int [0]> ""       
      9     9      2 <NA>       ]        ]             11       12         2            9       2         10 FALSE      FALSE     <NULL>   <int [0]> "\n"     

# load_json errors

    Code
      load_json()
    Condition
      Error in `load_json()`:
      ! Invalid arguments in `load_json()`: exactly one of `file` and `text` must be given.
    Code
      load_json(file = tempfile(), text = "foo")
    Condition
      Error in `load_json()`:
      ! Invalid arguments in `load_json()`: exactly one of `file` and `text` must be given.

