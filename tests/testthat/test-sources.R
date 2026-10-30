## Hygiene checks on the R this repository carries. These are scripts rather
## than a package, so nothing else looks at them: no R CMD check, and the manual
## build sources only build.R and install.R.

## `git -C` rather than changing directory, so the pathspec is resolved from the
## repository root whatever the caller's working directory is.
##
## The patterns are quoted because system2() runs through a shell on Unix: left
## bare, `*.R` is expanded by the shell in the working directory and git sees
## only the files already in it, never those in subdirectories.
trackedSources <- function(patterns = c("*.R", "*.Rmd")) {
  out <- system2("git", c("-C", shQuote(repoRoot()), "ls-files", shQuote(patterns)), stdout = TRUE)
  out <- out[nzchar(out)]
  ## this file holds the patterns it searches for, so it matches itself
  out[out != "tests/testthat/test-sources.R"]
}

## lines matching `pattern`, as "file:line", across the given files. Returns
## character(0) rather than NULL when nothing matches, which expect_setequal()
## requires -- it errors on NULL rather than treating it as empty.
grepSources <- function(files, pattern) {
  hits <- lapply(files, function(f) {
    p <- file.path(repoRoot(), f)
    if (!file.exists(p)) {
      return(character(0))
    }
    lines <- readLines(p, warn = FALSE)
    i <- grep(pattern, lines)
    if (length(i)) paste0(f, ":", i) else character(0)
  })
  as.character(unlist(hits))
}

test_that("no tracked R file leaves a browser() behind", {
  files <- trackedSources()
  skip_if_not(length(files) > 0, "no tracked R files")

  ## a breakpoint in a script that CI or a render runs waits for input that
  ## never arrives, so the job hangs until something kills it
  expect_setequal(
    grepSources(files, "(^|[^[:alnum:]._])browser[[:space:]]*\\("),
    character(0)
  )
})

test_that("no tracked source hardcodes a master branch", {
  files <- c(trackedSources(), "README.md", "_output.yml")
  skip_if_not(length(files) > 0, "no tracked sources")

  ## every repository in this organisation uses `main`. A master URL resolves
  ## through GitHub's rename redirect, so it keeps working while being wrong,
  ## and a repository without that history returns 404 instead.
  expect_setequal(
    grepSources(files, "/master/|branch:[[:space:]]*master|@master"),
    character(0)
  )
})

test_that("every tracked R file parses", {
  files <- trackedSources("*.R")
  skip_if_not(length(files) > 0, "no tracked .R files")

  ## the build sources build.R and install.R; a syntax error in either is
  ## otherwise found only once the build reaches it
  for (f in files) expect_no_error(parse(file.path(repoRoot(), f)))
})
