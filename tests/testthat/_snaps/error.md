# cnd

    Code
      cnd("foo {caller_arg(foo)} bar")
    Output
      <error in do(42): foo 42 bar>

# caller_arg

    Code
      caller_arg(foo)
    Output
      [[1]]
      [1] 100
      
      attr(,"class")
      [1] "tsjson_caller_arg"

# as_caller_arg

    Code
      as_caller_arg(42)
    Output
      [[1]]
      [1] 42
      
      attr(,"class")
      [1] "tsjson_caller_arg"

# check_named_arg

    Code
      f(42)
    Condition
      Error in `f()`:
      ! The `foobar` argument must be fully named.
    Code
      f(fooba = 42)
    Condition
      Error in `f()`:
      ! The `foobar` argument must be fully named.

# format_tsjson_parse_error_1 wide rows

    Code
      load_json(text = text)
    Condition
      Error in `token_table()`:
      ! JSON parse error `<text>`:1:96
      1| {"foofo...barbarbarbarbarbarbarbarbarb...arbarbarbarbarbarbarbar, ...123123 }
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# format.tsjson_parse_error

    Code
      format(err)
    Output
      [1] "<tsjson_parse_error>"                  
      [2] "JSON parse error `<text>`:1:9"         
      [3] "1| {\"key\": value, \"b\": 123123123 }"
      [4] "           ^^^^^^^"                    

# print.tsjson_parse_error

    Code
      print(err)
    Output
      <tsjson_parse_error>
      JSON parse error `<text>`:1:9
      1| {"key": value, "b": 123123123 }
                 ^^^^^^^

