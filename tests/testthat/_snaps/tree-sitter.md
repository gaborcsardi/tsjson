# sexpr_json

    Code
      sexpr_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
    Output
      [1] "(document (object (pair key: (string (string_content)) value: (true)) (pair key: (string (string_content)) value: (array (number) (number) (number)))))"

# sexpr_json errors

    Code
      sexpr_json()
    Condition
      Error in `sexpr_json()`:
      ! Invalid arguments in `sexpr_json()`: exactly one of `file` and `text` must be given.
    Code
      sexpr_json(text = "foo", file = "bar")
    Condition
      Error in `sexpr_json()`:
      ! Invalid arguments in `sexpr_json()`: exactly one of `file` and `text` must be given.

# sexpr_json from a file

    Code
      sexpr_json(file = tmp)
    Output
      [1] "(document (object (pair key: (string (string_content)) value: (true)) (pair key: (string (string_content)) value: (array (number) (number) (number)))))"

# token_table

    Code
      token_table(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
    Output
      # A data frame: 26 x 12
            id parent field_name type             code   start_byte end_byte start_row
         <int>  <int> <chr>      <chr>            <chr>       <int>    <int>     <int>
       1     1     NA <NA>       "document"        <NA>           0       29         0
       2     2      1 <NA>       "object"          <NA>           0       29         0
       3     3      2 <NA>       "{"              "{"             0        1         0
       4     4      2 <NA>       "pair"            <NA>           2       11         0
       5     5      4 key        "string"          <NA>           2        5         0
       6     6      5 <NA>       "\""             "\""            2        3         0
       7     7      5 <NA>       "string_content" "a"             3        4         0
       8     8      5 <NA>       "\""             "\""            4        5         0
       9     9      4 <NA>       ":"              ":"             5        6         0
      10    10      4 value      "true"           "true"          7       11         0
      # i 16 more rows
      # i 4 more variables: start_column <int>, end_row <int>, end_column <int>,
      #   children <I<list>>

# token_table errors

    Code
      token_table()
    Condition
      Error in `token_table()`:
      ! Invalid arguments in `token_table()`: exactly one of `file` and `text` must be given.
    Code
      token_table(text = "foo", file = "bar")
    Condition
      Error in `token_table()`:
      ! Invalid arguments in `token_table()`: exactly one of `file` and `text` must be given.

# token_table from a file

    Code
      token_table(file = tmp)
    Output
      # A data frame: 26 x 12
            id parent field_name type             code   start_byte end_byte start_row start_column end_row end_column children 
         <int>  <int> <chr>      <chr>            <chr>       <int>    <int>     <int>        <int>   <int>      <int> <I<list>>
       1     1     NA <NA>       "document"        <NA>           0       30         0            0       1          0 <int [1]>
       2     2      1 <NA>       "object"          <NA>           0       29         0            0       0         29 <int [5]>
       3     3      2 <NA>       "{"              "{"             0        1         0            0       0          1 <int [0]>
       4     4      2 <NA>       "pair"            <NA>           2       11         0            2       0         11 <int [3]>
       5     5      4 key        "string"          <NA>           2        5         0            2       0          5 <int [3]>
       6     6      5 <NA>       "\""             "\""            2        3         0            2       0          3 <int [0]>
       7     7      5 <NA>       "string_content" "a"             3        4         0            3       0          4 <int [0]>
       8     8      5 <NA>       "\""             "\""            4        5         0            4       0          5 <int [0]>
       9     9      4 <NA>       ":"              ":"             5        6         0            5       0          6 <int [0]>
      10    10      4 value      "true"           "true"          7       11         0            7       0         11 <int [0]>
      # i 16 more rows

# syntax_tree_json

    Code
      syntax_tree_json(text = "{ \"a\": true, \"b\": [1, 2, 3] }")
    Output
      document                  1|
      \-object                   |
        +-{                      |{
        +-pair                   |
        | +-string               |
        | | +-"                  |  "
        | | +-string_content     |   a
        | | \-"                  |    "
        | +-:                    |     :
        | \-true                 |       true
        +-,                      |           ,
        +-pair                   |
        | +-string               |
        | | +-"                  |             "
        | | +-string_content     |              b
        | | \-"                  |               "
        | +-:                    |                :
        | \-array                |
        |   +-[                  |                  [
        |   +-number             |                   1
        |   +-,                  |                    ,
        |   +-number             |                      2
        |   +-,                  |                       ,
        |   +-number             |                         3
        |   \-]                  |                          ]
        \-}                      |                            }
    Code
      text <- "{\n  \"a\": true,\n  \"b\": [\n    1,\n    2,\n    3\n  ]\n}"
      writeLines(text)
    Output
      {
        "a": true,
        "b": [
          1,
          2,
          3
        ]
      }
    Code
      syntax_tree_json(text = text)
    Output
      document                  1|
      \-object                   |
        +-{                      |{
        +-pair                  2|
        | +-string               |
        | | +-"                  |  "
        | | +-string_content     |   a
        | | \-"                  |    "
        | +-:                    |     :
        | \-true                 |       true
        +-,                      |           ,
        +-pair                  3|
        | +-string               |
        | | +-"                  |  "
        | | +-string_content     |   b
        | | \-"                  |    "
        | +-:                    |     :
        | \-array                |
        |   +-[                  |       [
        |   +-number            4|    1
        |   +-,                  |     ,
        |   +-number            5|    2
        |   +-,                  |     ,
        |   +-number            6|    3
        |   \-]                 7|  ]
        \-}                     8|}

# syntax_tree_json with hyperlinks

    Code
      syntax_tree_json(file = tmp)
    Output
      ]8;;file://<tempdir>/<tempfile>.json:1:1document]8;;                  1|
      \-]8;;file://<tempdir>/<tempfile>.json:1:1object]8;;                   |
        +-]8;;file://<tempdir>/<tempfile>.json:1:1{]8;;                      |{
        +-]8;;file://<tempdir>/<tempfile>.json:2:3pair]8;;                  2|
        | +-]8;;file://<tempdir>/<tempfile>.json:2:3string]8;;               |
        | | +-]8;;file://<tempdir>/<tempfile>.json:2:3"]8;;                  |  "
        | | +-]8;;file://<tempdir>/<tempfile>.json:2:4string_content]8;;     |   a
        | | \-]8;;file://<tempdir>/<tempfile>.json:2:5"]8;;                  |    "
        | +-]8;;file://<tempdir>/<tempfile>.json:2:6:]8;;                    |     :
        | \-]8;;file://<tempdir>/<tempfile>.json:2:8true]8;;                 |       true
        +-]8;;file://<tempdir>/<tempfile>.json:2:12,]8;;                      |           ,
        +-]8;;file://<tempdir>/<tempfile>.json:3:3pair]8;;                  3|
        | +-]8;;file://<tempdir>/<tempfile>.json:3:3string]8;;               |
        | | +-]8;;file://<tempdir>/<tempfile>.json:3:3"]8;;                  |  "
        | | +-]8;;file://<tempdir>/<tempfile>.json:3:4string_content]8;;     |   b
        | | \-]8;;file://<tempdir>/<tempfile>.json:3:5"]8;;                  |    "
        | +-]8;;file://<tempdir>/<tempfile>.json:3:6:]8;;                    |     :
        | \-]8;;file://<tempdir>/<tempfile>.json:3:8array]8;;                |
        |   +-]8;;file://<tempdir>/<tempfile>.json:3:8[]8;;                  |       [
        |   +-]8;;file://<tempdir>/<tempfile>.json:4:5number]8;;            4|    1
        |   +-]8;;file://<tempdir>/<tempfile>.json:4:6,]8;;                  |     ,
        |   +-]8;;file://<tempdir>/<tempfile>.json:5:5number]8;;            5|    2
        |   +-]8;;file://<tempdir>/<tempfile>.json:5:6,]8;;                  |     ,
        |   +-]8;;file://<tempdir>/<tempfile>.json:6:5number]8;;            6|    3
        |   \-]8;;file://<tempdir>/<tempfile>.json:7:3]]8;;                 7|  ]
        \-]8;;file://<tempdir>/<tempfile>.json:8:1}]8;;                     8|}

# query_json

    Code
      json <- format_selected(load_json(text = txt))
      json
    Output
      # json (5 lines)
      1 | {
      2 |   "a": 1,
      3 |   "b": "foo",
      4 |   "c": 20
      5 | }
    Code
      query_json(text = txt, query = "((pair value: (number) @num))")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                           match_count
        <int> <chr> <chr>                                   <int>
      1     1 <NA>  "((pair value: (number) @num))\n"           2
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          8        8         1            8       1          9 num   1    
      2     1       1     2         28       29         1           28       1         30 num   20   
      

# query_json errors

    Code
      query_json()
    Condition
      Error in `query_json()`:
      ! Invalid arguments in `query_json()`: exactly one of `file` and `text` must be given.
    Code
      query_json(text = "foo", file = "bar")
    Condition
      Error in `query_json()`:
      ! Invalid arguments in `query_json()`: exactly one of `file` and `text` must be given.

# query_json from a file

    Code
      query_json(file = tmp, query = "((pair value: (number) @num))")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                           match_count
        <int> <chr> <chr>                                   <int>
      1     1 <NA>  "((pair value: (number) @num))\n"           2
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          8        8         1            8       1          9 num   1    
      2     1       1     2         28       29         1           28       1         30 num   20   
      

