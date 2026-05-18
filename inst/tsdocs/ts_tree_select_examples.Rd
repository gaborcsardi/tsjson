#| title: JSONC examples
# ---
\subsection{Examples}{

\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{library(tsitter)
json <- ts_parse_jsonc(ts_serialize_jsonc(list(
  a = list(a1 = list(1,2,3), a2 = "string"),
  b = list(4, 5, 6),
  c = list(c1 = list("a", "b"))
)))
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#>                                                                                 
</pre></div>
}}


\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{json
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (21 lines)</span>                                                              
#> <span style="color: #6F7783;"> 1 | </span>\{                                                                          
#> <span style="color: #6F7783;"> 2 | </span>  "a": \{                                                                   
#> <span style="color: #6F7783;"> 3 | </span>    "a1": &#91;                                                                
#> <span style="color: #6F7783;"> 4 | </span>      1,                                                                   
#> <span style="color: #6F7783;"> 5 | </span>      2,                                                                   
#> <span style="color: #6F7783;"> 6 | </span>      3                                                                    
#> <span style="color: #6F7783;"> 7 | </span>    &#93;,                                                                     
#> <span style="color: #6F7783;"> 8 | </span>    "a2": "string"                                                         
#> <span style="color: #6F7783;"> 9 | </span>  \},                                                                       
#> <span style="color: #6F7783;">10 | </span>  "b": &#91;                                                                   
#> <span style="color: #6F7783;">ℹ 11 more lines</span>                                                                 
#> <span style="color: #6F7783;">ℹ Use `print(n = ...)` to see more lines</span>                                        
</pre></div>
}}


Select object by key:

\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{json |> ts_tree_select("a")
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (21 lines, 1 selected element)</span>                                          
#>   <span style="color: #6F7783;"> 1  | </span>\{                                                                       
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 2  | </span>  "a": <span style="color: #66C2CD;">\{</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 3  | </span><span style="color: #66C2CD;">    "a1": &#91;</span>                                                             
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 4  | </span><span style="color: #66C2CD;">      1,</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 5  | </span><span style="color: #66C2CD;">      2,</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 6  | </span><span style="color: #66C2CD;">      3</span>                                                                 
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 7  | </span><span style="color: #66C2CD;">    &#93;,</span>                                                                  
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 8  | </span><span style="color: #66C2CD;">    "a2": "string"</span>                                                      
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 9  | </span><span style="color: #66C2CD;">  \}</span>,                                                                    
#>   <span style="color: #6F7783;">10  | </span>  "b": &#91;                                                                
#>   <span style="color: #6F7783;">11  | </span>    4,                                                                  
#>   <span style="color: #6F7783;">12  | </span>    5,                                                                  
#>   <span style="color: #6F7783;">...</span>                                                                           
</pre></div>
}}


Select within select, these are the same:

\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{json |> ts_tree_select("a", "a1")
json |> ts_tree_select(list("a", "a1"))
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (21 lines, 1 selected element)</span>                                          
#>   <span style="color: #6F7783;"> 1  | </span>\{                                                                       
#>   <span style="color: #6F7783;"> 2  | </span>  "a": \{                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 3  | </span>    "a1": <span style="color: #66C2CD;">&#91;</span>                                                             
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 4  | </span><span style="color: #66C2CD;">      1,</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 5  | </span><span style="color: #66C2CD;">      2,</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 6  | </span><span style="color: #66C2CD;">      3</span>                                                                 
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 7  | </span><span style="color: #66C2CD;">    &#93;</span>,                                                                  
#>   <span style="color: #6F7783;"> 8  | </span>    "a2": "string"                                                      
#>   <span style="color: #6F7783;"> 9  | </span>  \},                                                                    
#>   <span style="color: #6F7783;">10  | </span>  "b": &#91;                                                                
#>   <span style="color: #6F7783;">...</span>                                                                           
#> <span style="color: #6F7783;"># jsonc (21 lines, 1 selected element)</span>                                          
#>   <span style="color: #6F7783;"> 1  | </span>\{                                                                       
#>   <span style="color: #6F7783;"> 2  | </span>  "a": \{                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 3  | </span>    "a1": <span style="color: #66C2CD;">&#91;</span>                                                             
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 4  | </span><span style="color: #66C2CD;">      1,</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 5  | </span><span style="color: #66C2CD;">      2,</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 6  | </span><span style="color: #66C2CD;">      3</span>                                                                 
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 7  | </span><span style="color: #66C2CD;">    &#93;</span>,                                                                  
#>   <span style="color: #6F7783;"> 8  | </span>    "a2": "string"                                                      
#>   <span style="color: #6F7783;"> 9  | </span>  \},                                                                    
#>   <span style="color: #6F7783;">10  | </span>  "b": &#91;                                                                
#>   <span style="color: #6F7783;">...</span>                                                                           
</pre></div>
}}


Select elements of an array. All elements:

\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{json |> ts_tree_select("b", TRUE)
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (21 lines, 3 selected elements)</span>                                         
#>   <span style="color: #6F7783;">...</span>                                                                           
#>   <span style="color: #6F7783;"> 8  | </span>    "a2": "string"                                                      
#>   <span style="color: #6F7783;"> 9  | </span>  \},                                                                    
#>   <span style="color: #6F7783;">10  | </span>  "b": &#91;                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">11  | </span>    <span style="color: #66C2CD;">4</span>,                                                                  
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">12  | </span>    <span style="color: #66C2CD;">5</span>,                                                                  
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">13  | </span>    <span style="color: #66C2CD;">6</span>                                                                   
#>   <span style="color: #6F7783;">14  | </span>  &#93;,                                                                    
#>   <span style="color: #6F7783;">15  | </span>  "c": \{                                                                
#>   <span style="color: #6F7783;">16  | </span>    "c1": &#91;                                                             
#>   <span style="color: #6F7783;">...</span>                                                                           
</pre></div>
}}


First two elements:

\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{json |> ts_tree_select("b", 1:2)
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (21 lines, 2 selected elements)</span>                                         
#>   <span style="color: #6F7783;">...</span>                                                                           
#>   <span style="color: #6F7783;"> 8  | </span>    "a2": "string"                                                      
#>   <span style="color: #6F7783;"> 9  | </span>  \},                                                                    
#>   <span style="color: #6F7783;">10  | </span>  "b": &#91;                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">11  | </span>    <span style="color: #66C2CD;">4</span>,                                                                  
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">12  | </span>    <span style="color: #66C2CD;">5</span>,                                                                  
#>   <span style="color: #6F7783;">13  | </span>    6                                                                   
#>   <span style="color: #6F7783;">14  | </span>  &#93;,                                                                    
#>   <span style="color: #6F7783;">15  | </span>  "c": \{                                                                
#>   <span style="color: #6F7783;">...</span>                                                                           
</pre></div>
}}


First and last elements:

\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{json |> ts_tree_select("b", c(1, -1))
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (21 lines, 2 selected elements)</span>                                         
#>   <span style="color: #6F7783;">...</span>                                                                           
#>   <span style="color: #6F7783;"> 8  | </span>    "a2": "string"                                                      
#>   <span style="color: #6F7783;"> 9  | </span>  \},                                                                    
#>   <span style="color: #6F7783;">10  | </span>  "b": &#91;                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">11  | </span>    <span style="color: #66C2CD;">4</span>,                                                                  
#>   <span style="color: #6F7783;">12  | </span>    5,                                                                  
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">13  | </span>    <span style="color: #66C2CD;">6</span>                                                                   
#>   <span style="color: #6F7783;">14  | </span>  &#93;,                                                                    
#>   <span style="color: #6F7783;">15  | </span>  "c": \{                                                                
#>   <span style="color: #6F7783;">16  | </span>    "c1": &#91;                                                             
#>   <span style="color: #6F7783;">...</span>                                                                           
</pre></div>
}}


Regular expressions:

\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{json |> ts_tree_select(c("a", "c"), regex = "1$")
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (21 lines, 2 selected elements)</span>                                         
#>   <span style="color: #6F7783;"> 1  | </span>\{                                                                       
#>   <span style="color: #6F7783;"> 2  | </span>  "a": \{                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 3  | </span>    "a1": <span style="color: #66C2CD;">&#91;</span>                                                             
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 4  | </span><span style="color: #66C2CD;">      1,</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 5  | </span><span style="color: #66C2CD;">      2,</span>                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 6  | </span><span style="color: #66C2CD;">      3</span>                                                                 
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;"> 7  | </span><span style="color: #66C2CD;">    &#93;</span>,                                                                  
#>   <span style="color: #6F7783;"> 8  | </span>    "a2": "string"                                                      
#>   <span style="color: #6F7783;"> 9  | </span>  \},                                                                    
#>   <span style="color: #6F7783;">10  | </span>  "b": &#91;                                                                
#>   <span style="color: #6F7783;">...</span>                                                                           
#>   <span style="color: #6F7783;">13  | </span>    6                                                                   
#>   <span style="color: #6F7783;">14  | </span>  &#93;,                                                                    
#>   <span style="color: #6F7783;">15  | </span>  "c": \{                                                                
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">16  | </span>    "c1": <span style="color: #66C2CD;">&#91;</span>                                                             
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">17  | </span><span style="color: #66C2CD;">      "a",</span>                                                              
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">18  | </span><span style="color: #66C2CD;">      "b"</span>                                                               
#> <span style="background-color: #66C2CD;">&gt;</span> <span style="color: #6F7783;">19  | </span><span style="color: #66C2CD;">    &#93;</span>                                                                   
#>   <span style="color: #6F7783;">20  | </span>  \}                                                                     
#>   <span style="color: #6F7783;">21  | </span>\}                                                                       
</pre></div>
}}

}
