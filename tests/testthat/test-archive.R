test_that("every archived PDF is named so its version can be read back", {
  pdfs <- archivedPDFs()

  ## publishManualArchive() parses the version out of the file name with
  ## "-v<version>.pdf" and orders the index by it; a name it cannot parse
  ## sorts last instead of failing, so nothing else would report this
  expect_true(all(grepl("-v[0-9]+([.][0-9]+)+[.]pdf$", pdfs)))
})

test_that("archived versions are all distinct", {
  pdfs <- archivedPDFs()
  versions <- sub("^.*-v(.*)[.]pdf$", "\\1", pdfs)

  expect_setequal(versions[duplicated(versions)], character(0))
})

test_that("archived versions parse as versions", {
  pdfs <- archivedPDFs()
  versions <- sub("^.*-v(.*)[.]pdf$", "\\1", pdfs)

  expect_false(any(is.na(numeric_version(versions, strict = FALSE))))
})

test_that("the archive carries no version newer than the manual's", {
  pdfs <- archivedPDFs()
  versions <- numeric_version(sub("^.*-v(.*)[.]pdf$", "\\1", pdfs), strict = FALSE)
  current <- numeric_version(
    read.dcf(file.path(repoRoot(), "DESCRIPTION"), fields = "Version")[1]
  )

  ## an archived PDF ahead of DESCRIPTION means the version was bumped down, or
  ## a release was archived from a branch that is not this one
  expect_false(any(versions > current, na.rm = TRUE))
})

test_that("archived PDFs share one prefix", {
  pdfs <- archivedPDFs()
  prefixes <- unique(sub("-v[0-9].*$", "", pdfs))

  ## the prefix comes from build.R; changing it strands the older files in a
  ## second series that the index lists alongside the first
  expect_length(prefixes, 1L)
})
