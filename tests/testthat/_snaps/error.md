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

