pkgPath <- file.path("packages", version$platform,
                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))
.libPaths(pkgPath)   ## need to include.side = TRUE to use bookdown and rmarkdown

# example R options set globally
options("width" = 60
        , repos = c(CRAN = "https://cran.rstudio.com")
        )

## knitr-related options
options(knitr.table.format = function() {
  if (knitr::is_latex_output())
    "latex" else "pipe"
})

options("knitr.graphics.rel_path" = FALSE)

rm(list = ls(all.names = TRUE))

# options(bookdown.post.latex = function(x) {
#   # substitute nonbreaking spaces in \texttt{} with normal spaces
#   m = gregexpr('\\\\texttt\\{[^}]+}', x)
#   regmatches(x, m) = lapply(regmatches(x, m), function(z) {
#     gsub('\\\\ ', ' ', z)
#   })
#   # only build a skeleton for the online version
#   if (Sys.getenv('BOOKDOWN_FULL_PDF', '') == 'false') return(bookdown:::strip_latex_body(
#     x, '\nThis PDF is only a skeleton. Please either read the free online HTML version, or purchase a hard-copy of this book.\n'
#   ))
#   # fix syntax highlighting:
#   # \FunctionTok{tufte:}\AttributeTok{:tufte_html: default} ->
#   # \FunctionTok{tufte::tufte_html:}\AttributeTok{ default}
#   x = gsub('(\\\\AttributeTok\\{[^:]+:)(})(\\\\FunctionTok\\{)(:[^:]+:)', '\\1\\4\\2\\3', x)
#   if (length(i <- grep('^\\\\begin\\{longtable\\}', x)) == 0) return(x)
#   i1 = bookdown:::next_nearest(i, which(x == '\\toprule'))
#   i2 = bookdown:::next_nearest(i, which(x == '\\endfirsthead'))
#   x[i1 - 1] = paste0(x[i1 - 1], '\n\\begin{tabular}{', gsub('[^lcr]', '', gsub('.*\\[]', '', x[i])), '}')
#   x[i] = '\\begin{table}'
#   x[x == '\\end{longtable}'] = '\\end{tabular}\n\\end{table}'
#   x[x == '\\endhead'] = ''
#   x = x[-unlist(mapply(seq, i1, i2, SIMPLIFY = FALSE))]
#   x
# })

# chunk options set globally
knitr::opts_chunk$set(
  tidy = TRUE,
  tidy.opts = list(width.cutoff = 60),
  size = "tiny",
  fig.pos = "H",
  out.extra = ""
)



