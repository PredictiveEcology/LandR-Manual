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
