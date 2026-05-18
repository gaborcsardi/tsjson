#| title: JSONC examples
# ---
\if{html}{\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\preformatted{jsonc <- tsitter::ts_tree_new(
  tsjsonc::ts_language_jsonc(),
  text = "\{ \\"a\\": true, // comment\\n \\"b\\": [1, 2, 3], \}"
)
jsonc
}\if{html}{\out{</div>}}\if{html}{\out{
<div class="asciicast" style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000"><pre>
#> <span style="color: #6F7783;"># jsonc (2 lines)</span>                                                               
#> <span style="color: #6F7783;">1 | </span>\{ "a": true, // comment                                                     
#> <span style="color: #6F7783;">2 | </span> "b": &#91;1, 2, 3&#93;, \}                                                          
</pre></div>
}}
