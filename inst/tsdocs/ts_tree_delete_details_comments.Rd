#| title: 
# ---
\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{jsonc <- tsjsonc::ts_parse_jsonc(
  "// top comment\\n\{ \\"a\\": // comment\\n  true,\\n \\"b\\": [1, 2, 3] \}"
) |> tsitter::ts_tree_format()
jsonc
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (11 lines)</span>                                                              
#> <span style="color: #6F7783;"> 1 | </span>// top comment                                                             
#> <span style="color: #6F7783;"> 2 | </span>\{                                                                          
#> <span style="color: #6F7783;"> 3 | </span>    "a":                                                                   
#> <span style="color: #6F7783;"> 4 | </span>        // comment                                                         
#> <span style="color: #6F7783;"> 5 | </span>        true,                                                              
#> <span style="color: #6F7783;"> 6 | </span>    "b": &#91;                                                                 
#> <span style="color: #6F7783;"> 7 | </span>        1,                                                                 
#> <span style="color: #6F7783;"> 8 | </span>        2,                                                                 
#> <span style="color: #6F7783;"> 9 | </span>        3                                                                  
#> <span style="color: #6F7783;">10 | </span>    &#93;                                                                      
#> <span style="color: #6F7783;">ℹ 1 more line</span>                                                                   
#> <span style="color: #6F7783;">ℹ Use `print(n = ...)` to see more lines</span>                                        
</pre></div>
}}


\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{jsonc |> ts_tree_select("a") |> ts_tree_delete()
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (8 lines)</span>                                                               
#> <span style="color: #6F7783;">1 | </span>// top comment                                                              
#> <span style="color: #6F7783;">2 | </span>\{                                                                           
#> <span style="color: #6F7783;">3 | </span>    "b": &#91;                                                                  
#> <span style="color: #6F7783;">4 | </span>        1,                                                                  
#> <span style="color: #6F7783;">5 | </span>        2,                                                                  
#> <span style="color: #6F7783;">6 | </span>        3                                                                   
#> <span style="color: #6F7783;">7 | </span>    &#93;                                                                       
#> <span style="color: #6F7783;">8 | </span>\}                                                                           
</pre></div>
}}
