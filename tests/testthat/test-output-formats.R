test_that("the book declares an HTML format", {
  cfg <- outputConfig()

  expect_true(any(grepl("bs4_book|gitbook|html_book", names(cfg))))
})

test_that("the PDF is declared after the EPUB", {
  cfg <- outputConfig()
  pdf <- grep("pdf_book", names(cfg))
  epub <- grep("epub_book", names(cfg))
  skip_if_not(length(pdf) == 1 && length(epub) == 1, "not both formats declared")

  ## bookdown writes the formats in declaration order and a failure stops the
  ## rest, so the format most likely to fail goes last
  expect_gt(pdf, epub)
})

test_that("bs4_book does not set download:, which it ignores", {
  cfg <- outputConfig()
  i <- grep("bs4_book", names(cfg))
  skip_if_not(length(i) == 1, "no bs4_book format")
  bs4 <- cfg[[i]]

  ## `download` is a gitbook option. Under bs4_book it is accepted and does
  ## nothing, so the PDF and EPUB are published with nothing linking to them
  expect_null(bs4$download)
})

test_that("the output directory matches what the deploy publishes", {
  cfg <- bookdownConfig()
  wf <- manualWorkflow()
  folder <- grep("^\\s*folder:", wf, value = TRUE)
  skip_if_not(length(folder) == 1, "no single deploy folder")
  folder <- trimws(sub("^\\s*folder:\\s*", "", folder))

  ## the workflow deploys the contents of this directory; if the two name
  ## different places the build succeeds and the site does not change.
  ## Compared as paths: "./docs" and "docs" are the same directory.
  expect_identical(
    normalizePath(file.path(repoRoot(), cfg$output_dir), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(repoRoot(), folder), winslash = "/", mustWork = FALSE)
  )
})

test_that("the LaTeX preamble the PDF declares exists", {
  cfg <- outputConfig()
  i <- grep("pdf_book", names(cfg))
  skip_if_not(length(i) == 1, "no pdf_book format")
  pdf <- cfg[[i]]
  skip_if_not(!is.null(pdf$includes$in_header), "no preamble declared")

  expect_true(file.exists(file.path(repoRoot(), pdf$includes$in_header)))
})

test_that("the document class the book names is available", {
  f <- file.path(repoRoot(), "index.Rmd")
  skip_if_not(file.exists(f))
  cls <- rmarkdown::yaml_front_matter(f)$documentclass
  skip_if_not(!is.null(cls), "no documentclass")

  ## krantz.cls is carried in the repository, not supplied by TeX Live
  expect_true(file.exists(file.path(repoRoot(), paste0(cls, ".cls"))))
})

test_that("the repo branch the book links to is one the workflow builds", {
  cfg <- outputConfig()
  i <- grep("bs4_book|gitbook", names(cfg))
  skip_if_not(length(i) == 1, "no HTML format with a repo block")
  branch <- cfg[[i]]$repo$branch
  skip_if_not(!is.null(branch), "no repo branch declared")

  ## the "view source" links are built from this. A branch that was renamed
  ## still resolves through GitHub's redirect, so nothing breaks visibly and
  ## the stale name stays.
  wf <- manualWorkflow()
  built <- regmatches(wf, regexpr("(?<=branches: \\[)[^]]+", wf, perl = TRUE))
  built <- trimws(strsplit(paste(unlist(built), collapse = ","), ",")[[1]])
  skip_if_not(length(built) > 0, "workflow declares no push branches")

  expect_contains(built, branch)
})
