# get_language error

    Code
      .Call(c_s_expr, "input", 1L, NULL)
    Condition
      Error:
      ! Unknonwn tree-sitter language code

# get_ranges

    Code
      sexpr_json(text = text1, ranges = ranges)
    Output
      [1] "(document (object (pair key: (string (string_content)) value: (true)) (pair key: (string (string_content)) value: (array (number) (number) (number)))))"

---

    Code
      sexpr_json(text = text1, ranges = list(c(1L, 10L), c(111L, nchar(text1))))
    Condition
      Error:
      ! Invalid ranges, must be a data frame of 6 integer columns

---

    Code
      sexpr_json(text = text1, ranges = badranges)
    Condition
      Error:
      ! Invalid ranges, must be a data frame of 6 integer columns

---

    Code
      sexpr_json(text = text, ranges = badranges2)
    Condition
      Error:
      ! Invalid ranges for tree-sitter parser

# token_table

    Code
      token_table(text = text, ranges = badranges2)
    Condition
      Error:
      ! Invalid ranges for tree-sitter parser

# check_predicate_eq

    Code
      query_json(text = text, query = "((pair key: (string (string_content) @key) (#eq? @key \"a\")))")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                            match_count
        <int> <chr> <chr>                                                                    <int>
      1     1 <NA>  "((pair key: (string (string_content) @key) (#eq? @key \"a\")))\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          4        4         1            4       1          5 key   a    
      

# check_predicate_eq 2

    Code
      query_json(text = text, query = "((pair key: (string (string_content) @key)\n                 value: (string (string_content) @val)\n                 (#eq? @key @val)\n               ))")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "((pair key: (string (string_content) @key)\n                 value: (string (string_content) @val)\n                 (#eq? @key @val)\n               ))\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1         24       24         1           24       1         25 key   x    
      2     2       1     1         29       29         1           29       1         30 val   x    
      

