## clean workspace
rm(list = ls())

# example R options set globally
options("width" = 60,
        "repos" = c(CRAN = "https://cran.rstudio.com"))

# chunk options set globally
knitr::opts_chunk$set(
  tidy = TRUE,
  tidy.opts = list(width.cutoff = 60)
)
