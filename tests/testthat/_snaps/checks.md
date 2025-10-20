# as_flag

    Code
      as_flag(v1)
    Condition
      Error:
      ! Invalid argument: `v1` must a flag (logical scalar), but it is a number.
    Code
      as_flag(v2)
    Condition
      Error:
      ! Invalid argument: `v2` must a flag (logical scalar), but it is a character vector.

# as_count

    Code
      as_count(v1)
    Condition
      Error:
      ! Invalid argument: `v1` must be an integer scalar, not a vector.
    Code
      as_count(v2)
    Condition
      Error:
      ! Invalid argument: `v2` must not be `NA`.
    Code
      as_count(v3)
    Condition
      Error:
      ! Invalid argument: `v3` must be non-negative.
    Code
      as_count(v4)
    Condition
      Error:
      ! Invalid argument: `v4` must be a non-negative integer scalar, but it is a data frame.

# as_choice

    Code
      as_choice(v1, letters[1:5])
    Condition
      Error:
      ! Invalid argument: `v1` must be one of 'a', 'b', 'c', 'd', 'e', but it is 'z'.

---

    Code
      as_choice(v2, letters[1:5])
    Condition
      Error:
      ! Invalid argument: `v2` must be a string scalar, one of 'a', 'b', 'c', 'd', 'e', but it is an integer vector.

# default options

    Code
      as_tsjson_options(NULL)
    Output
      $allow_empty_content
      [1] TRUE
      
      $allow_comments
      [1] TRUE
      
      $format
      [1] "pretty"
      
      $indent_width
      [1] 4
      
      $indent_style
      [1] "space"
      

# as_tsjson_options

    Code
      as_tsjson_options(list(format = "compact", allow_comments = FALSE))
    Output
      $format
      [1] "compact"
      
      $allow_comments
      [1] FALSE
      
      $allow_empty_content
      [1] TRUE
      
      $indent_width
      [1] 4
      
      $indent_style
      [1] "space"
      

---

    Code
      as_tsjson_options(v1)
    Condition
      Error:
      ! Invalid argument: `v1` must be a named list of tsjson options (see `?tsjson_options`), but it is a function.
    Code
      as_tsjson_options(v2)
    Condition
      Error:
      ! Invalid argument: `v2` must be a named list of tsjson options (see `?tsjson_options`), but not all of its entries are named.
    Code
      as_tsjson_options(v3)
    Condition
      Error:
      ! Invalid argument: `v3` contains unknown tsjson option: `foo`. Known tsjson options are: `allow_empty_content`, `allow_comments`, `format`, `indent_width`, and `indent_style`.
    Code
      as_tsjson_options(v4)
    Condition
      Error:
      ! Invalid argument: `v4[["allow_empty_content"]]` must a flag (logical scalar), but it is a string.
    Code
      as_tsjson_options(v5)
    Condition
      Error:
      ! Invalid argument: `v5[["allow_comments"]]` must a flag (logical scalar), but it is a string.
    Code
      as_tsjson_options(v6)
    Condition
      Error:
      ! Invalid argument: `v6[["format"]]` must be one of 'pretty', 'compact', 'oneline', but it is 'fancy'.
    Code
      as_tsjson_options(v7)
    Condition
      Error:
      ! Invalid argument: `v7[["indent_width"]]` must be non-negative.
    Code
      as_tsjson_options(v8)
    Condition
      Error:
      ! Invalid argument: `v8[["indent_style"]]` must be one of 'space', 'tab', but it is 'fancy'.

