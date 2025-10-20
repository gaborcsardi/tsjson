# unserialize_selected

    Code
      unserialize_selected(json)
    Output
      [[1]]
      [[1]]$a
      [1] 1
      
      [[1]]$b
      [[1]]$b[[1]]
      [1] 1
      
      [[1]]$b[[2]]
      [1] 2
      
      [[1]]$b[[3]]
      [1] 3
      
      
      

# unserialize_false

    Code
      unserialize_selected(select(json, "a"))
    Output
      [[1]]
      [1] FALSE
      

