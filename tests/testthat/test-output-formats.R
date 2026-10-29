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
  bs4 <- cfg[[grep("bs4_book", names(cfg))[[1]]]]
  skip_if_not(!is.null(bs4), "no bs4_book format")

  ## `download` is a gitbook option. Under bs4_book it is accepted and does
  ## nothing, so the PDF and EPUB are published with nothing linking to them
  expect_null(bs4$download)
})

test_that("the output directory matches what the deploy publishes", {
  cfg <- bookdownConfig()

  ## the workflow deploys the contents of this directory; if the two disagree,
  ## the build succeeds and the site does not change
  expect_identical(cfg$output_dir, "docs")

  wf <- manualWorkflow()
  folder <- grep("^\\s*folder:", wf, value = TRUE)
  skip_if_not(length(folder) == 1, "no single deploy folder")
  expect_identical(trimws(sub("^\\s*folder:\\s*", "", folder)), cfg$output_dir)
})

test_that("the LaTeX preamble the PDF declares exists", {
  cfg <- outputConfig()
  pdf <- cfg[[grep("pdf_book", names(cfg))[[1]]]]
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
