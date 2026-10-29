## Adding a module to this manual takes edits in separate files, and the build
## is green whichever one is missed: a module fetched without a chapter entry is
## staged and silently left out of the book, and a chapter entry without a
## fetch fails only once the render reaches it.

test_that("every module the workflow fetches has a chapter", {
  fetched <- workflowModules()
  charted <- chapterModules()

  expect_setequal(setdiff(fetched, charted), character(0))
})

test_that("every chapter has a module the workflow fetches", {
  fetched <- workflowModules()
  charted <- chapterModules()

  expect_setequal(setdiff(charted, fetched), character(0))
})

test_that("the chapter list and the fetch list name the same modules", {
  ## the two tests above say which direction is wrong; this one is the invariant
  expect_setequal(workflowModules(), chapterModules())
})

test_that("a module pinned off MODULE_REF says why", {
  refs <- workflowModuleRefs()
  skip_if_not(length(refs) > 0, "no module checkouts found")

  ## pinning is sometimes necessary: a module with no `development` branch
  ## cannot be fetched at MODULE_REF. Unexplained, it is indistinguishable from
  ## a module whose branch quietly stopped being tracked, which is how one
  ## manual carried a stale exception for a year.
  pinned <- Filter(function(x) !grepl("MODULE_REF", x$ref, fixed = TRUE), refs)
  undocumented <- names(Filter(function(x) !x$explained, pinned))

  expect_setequal(undocumented, character(0))
})

test_that("no module is fetched twice", {
  fetched <- workflowModules()

  expect_setequal(fetched[duplicated(fetched)], character(0))
})

test_that("the chapter list has no duplicate entries", {
  cfg <- bookdownConfig()

  expect_setequal(cfg$rmd_files[duplicated(cfg$rmd_files)], character(0))
})

test_that("index.Rmd is the book's first chapter", {
  cfg <- bookdownConfig()
  ## bookdown takes the book's title page and YAML from the first file
  expect_identical(cfg$rmd_files[[1L]], "index.Rmd")
})

test_that("every chapter the book lists exists or is generated", {
  cfg <- bookdownConfig()
  listed <- cfg$rmd_files
  ## the staged ones are build output; the rest are tracked files
  tracked <- listed[!grepl("_manual_rmds/", listed)]
  missing <- tracked[!file.exists(file.path(repoRoot(), tracked))]

  expect_setequal(missing, character(0))
})

test_that("the landing page lists the modules the book has chapters for", {
  linked <- indexModuleLinks()
  charted <- chapterModules()
  ## only some manuals keep such a list; where one exists it is hand-maintained,
  ## so a module can be fetched, charted, and still missing from the overview
  listed <- intersect(linked, charted)
  skip_if_not(length(listed) > 0, "no module list on the landing page")

  expect_setequal(setdiff(charted, linked), character(0))
})
