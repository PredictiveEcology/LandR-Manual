test_that("DESCRIPTION carries a parseable Version", {
  v <- read.dcf(file.path(repoRoot(), "DESCRIPTION"), fields = "Version")[1]

  ## build.R puts this on the title page and archiveManualPDF() names the
  ## released PDF after it, refusing an empty one
  expect_false(is.na(v))
  expect_match(v, "^[0-9]+([.-][0-9]+)+$")
  expect_s3_class(numeric_version(v), "numeric_version")
})

test_that("the SpaDES.docs dependency is pinned to a released version", {
  d <- read.dcf(file.path(repoRoot(), "DESCRIPTION"))
  remotes <- if ("Remotes" %in% colnames(d)) d[1, "Remotes"] else ""
  pin <- regmatches(remotes, regexpr("PredictiveEcology/SpaDES\\.docs@[^,\\s]+", remotes))
  skip_if_not(length(pin) == 1, "SpaDES.docs not in Remotes")

  ## tracking a branch means the manual's own build changes without a commit here
  expect_match(pin, "@v[0-9]+[.][0-9]+[.][0-9]+$")
})

test_that("the bibliography and CSL the book declares are generated or tracked", {
  y <- rmarkdown::yaml_front_matter(file.path(repoRoot(), "index.Rmd"))
  declared <- c(unlist(y$bibliography), y$csl)
  skip_if_not(length(declared) > 0, "no bibliography declared")

  ## these live under citations/, which build.R generates, so the test asserts
  ## the declaration is well formed rather than that the files are present
  expect_true(all(grepl("^citations/", declared)))
  expect_true(all(grepl("[.](bib|csl)$", declared)))
})

test_that("the manual's version appears in the README news list", {
  v <- read.dcf(file.path(repoRoot(), "DESCRIPTION"), fields = "Version")[1]
  readme <- readLines(file.path(repoRoot(), "README.md"), warn = FALSE)
  news <- grep("^-\\s+v[.]?\\s*[0-9]", readme, value = TRUE)
  skip_if_not(length(news) > 0, "no news list in README")

  ## a release whose version is not described is a release nobody can identify
  expect_match(paste(news, collapse = "\n"), v, fixed = TRUE)
})

test_that("GitHub dependency specs are in Remotes, not a dependency field", {
  d <- read.dcf(file.path(repoRoot(), "DESCRIPTION"))
  fields <- intersect(c("Depends", "Imports", "Suggests", "LinkingTo"), colnames(d))
  entries <- unlist(lapply(fields, function(f) strsplit(d[1, f], ",\\s*")[[1]]))
  entries <- trimws(entries[!is.na(entries)])

  ## only Remotes is read for them. An `owner/repo@ref` under Imports or
  ## Suggests is an invalid package name there, so the pin it looks like it
  ## applies is inert and the version actually installed is whatever else
  ## provided it.
  misplaced <- grep("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", entries, value = TRUE)

  expect_setequal(misplaced, character(0))
})

test_that("the R version constraint is a floor, not an equality", {
  d <- read.dcf(file.path(repoRoot(), "DESCRIPTION"))
  skip_if_not("Depends" %in% colnames(d), "no Depends field")
  r <- grep("^R\\s*\\(", trimws(strsplit(d[1, "Depends"], ",\\s*")[[1]]), value = TRUE)
  skip_if_not(length(r) == 1, "no R constraint")

  ## `R (== 4.4)` refuses every other version, including the one CI runs
  expect_no_match(r, "==", fixed = TRUE)
})

test_that("the license is set", {
  d <- read.dcf(file.path(repoRoot(), "DESCRIPTION"))
  skip_if_not("License" %in% colnames(d), "no License field")

  ## usethis leaves a placeholder naming the functions that would set one
  expect_no_match(d[1, "License"], "use_.*_license", fixed = FALSE)
})

test_that("every environment variable index.Rmd reads is one build.R sets", {
  read <- indexEnvVars()
  skip_if_not(length(read) > 0, "index.Rmd reads no environment variables")
  provided <- buildEnvVars()

  ## the title page's subtitle comes from one of these. A name nothing sets
  ## yields "", so the manual renders with "v." and no version after it and
  ## nothing else reports a problem.
  expect_contains(provided, read)
})
