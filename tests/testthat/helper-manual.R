## Readers for the three places a module has to be named, and for the book's
## output configuration. Each returns what is actually in the file, so a test
## asserts on the manual's real configuration rather than on a copy of it.
##
## testthat runs with the working directory at tests/testthat, so everything is
## resolved from the repository root.

repoRoot <- function() {
  normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

## found by content, not by name: each manual names its workflow after itself
manualWorkflow <- function() {
  wf <- list.files(
    file.path(repoRoot(), ".github", "workflows"),
    pattern = "[.]ya?ml$", full.names = TRUE
  )
  builds <- Filter(function(f) any(grepl("path: modules/", readLines(f, warn = FALSE))), wf)
  testthat::skip_if_not(length(builds) > 0, "no workflow fetches modules")
  testthat::expect_length(builds, 1L)
  readLines(builds[[1L]], warn = FALSE)
}

## the modules the workflow checks out, from the `path: modules/<name>` of each
## actions/checkout step
workflowModules <- function(lines = manualWorkflow()) {
  m <- regmatches(lines, regexpr("(?<=path: modules/)\\S+", lines, perl = TRUE))
  sort(unlist(m, use.names = FALSE), method = "radix")
}

## the ref each module checkout uses, and whether a deviation from the shared
## MODULE_REF is explained by a comment above the step. A module may genuinely
## need pinning -- one with no `development` branch cannot be checked out at
## MODULE_REF -- but an unexplained pin is indistinguishable from a module whose
## branch silently stopped being tracked.
workflowModuleRefs <- function(lines = manualWorkflow()) {
  idx <- grep("^\\s*repository:\\s*PredictiveEcology/", lines)
  out <- lapply(idx, function(i) {
    window <- lines[seq(i, min(i + 3L, length(lines)))]
    ref <- grep("^\\s*ref:", window, value = TRUE)
    before <- lines[seq(max(1L, i - 6L), i)]
    list(
      ref = if (length(ref)) trimws(sub("^\\s*ref:\\s*", "", ref[[1]])) else NA_character_,
      explained = any(grepl("^\\s*##", before))
    )
  })
  names(out) <- sub("^.*PredictiveEcology/", "", trimws(lines[idx]))
  out
}

bookdownConfig <- function() {
  f <- file.path(repoRoot(), "_bookdown.yml")
  testthat::skip_if_not(file.exists(f), "no _bookdown.yml")
  yaml::read_yaml(f)
}

## the modules the book actually has chapters for: _manual_rmds/<name>2.Rmd
chapterModules <- function(cfg = bookdownConfig()) {
  staged <- grep("_manual_rmds/", cfg$rmd_files, value = TRUE)
  sort(sub("2\\.Rmd$", "", basename(staged)), method = "radix")
}

outputConfig <- function() {
  f <- file.path(repoRoot(), "_output.yml")
  testthat::skip_if_not(file.exists(f), "no _output.yml")
  yaml::read_yaml(f)
}

## the module repositories the landing page links, where it keeps such a list.
## Not every manual does; the caller skips when this is empty.
indexModuleLinks <- function() {
  f <- file.path(repoRoot(), "index.Rmd")
  testthat::skip_if_not(file.exists(f), "no index.Rmd")
  lines <- readLines(f, warn = FALSE)
  m <- regmatches(
    lines,
    gregexpr("(?<=github\\.com/PredictiveEcology/)[A-Za-z0-9_.-]+", lines, perl = TRUE)
  )
  sort(unique(unlist(m, use.names = FALSE)), method = "radix")
}

archivedPDFs <- function() {
  d <- file.path(repoRoot(), "archive", "pdf")
  testthat::skip_if_not(dir.exists(d), "no archived PDFs in this repository")
  list.files(d, pattern = "[.]pdf$")
}
