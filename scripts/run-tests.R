## Run the manual's testthat suite the same way CI does.
##
## There is no package here, so nothing selects a testthat edition:
## TESTTHAT_EDITION=3 turns edition 3 on for every file. NOT_CRAN=true is needed
## as well, or expect_snapshot() skips with "On CRAN" and a snapshot test
## "passes" by not running.
##
## Nothing is sourced first. These tests read the manual's own configuration --
## the workflow, _bookdown.yml, _output.yml, index.Rmd, DESCRIPTION and
## archive/pdf -- and assert that those agree with each other. They do not
## render the book, fetch a module or touch any data, so they need only
## testthat, yaml and rmarkdown.
##
##   Rscript scripts/run-tests.R                  # everything
##   Rscript scripts/run-tests.R chapters         # files matching a filter

Sys.setenv(TESTTHAT_EDITION = "3", NOT_CRAN = "true")
filter <- commandArgs(trailingOnly = TRUE)

testthat::test_dir(
  "tests/testthat",
  filter = if (length(filter)) filter[[1L]] else NULL,
  stop_on_failure = TRUE
)
