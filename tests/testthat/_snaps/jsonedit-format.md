# formatting retains comments

    Code
      writeLines(format_json(text = text))
    Output
      {
          // a comment
          "a": 1, // another one
          "b": {
              "c": 2
          }
      } // trailing

