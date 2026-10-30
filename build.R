## This manual must be knitted by running this script.
## Package installation happens in install.R, ahead of it.

library(bibtex)
library(bookdown)
library(data.table)
library(knitr)
library(RefManageR)
library(SpaDES.docs)



paths <- manualPaths()

## references ---------------------------------------

writePkgBib(file.path(paths$citations, "packages.bib"))

downloadCSL("ecology-letters", paths$citations)

## index.Rmd cites the SpaDES package as @ChubatyMcIntire2019, but writePkgBib()
## keys package entries by package name (R-SpaDES), so that key resolves to
## nothing. Take the citation from the package itself rather than transcribing
## it, and file it under the key the prose uses.
spadesEntry <- utils::toBibtex(utils::citation("SpaDES"))
spadesEntry[1] <- sub("\\{[^,]*,", "{ChubatyMcIntire2019,", spadesEntry[1])
writeLines(spadesEntry, file.path(paths$citations, "references_SpaDES.bib"))

## referencesLandRManual.bib is OUTPUT ONLY, assembled from the module
## bibliographies plus the two generated above.
##
## It used to be its own input as well, on the premise that the manual
## accumulated references cited outside any module chapter. It never had any:
## the only citation in the manual's own files is @ChubatyMcIntire2019, which
## references_SpaDES.bib supplies from the installed package. What the pattern
## did instead was merge the file into itself on every local build -- the copy
## found in October 2026 held 265 keys, 216 of them RefManageR's `:1`-style
## duplicate markers. CI never saw that, because citations/ is generated and
## starts empty there, which is also why nothing was ever lost by it.
##
## Should the manual gain references of its own, put them in a tracked
## references_manual.bib and add it to extraBibs, as the LandWeb manual does.
collapseModuleBibs(
  modulePath = file.path(paths$prj, "modules"),
  extraBibs = file.path(paths$citations,
                        c("references_SpaDES.bib", "packages.bib")),
  outFile = file.path(paths$citations, "referencesLandRManual.bib")
)

## badge images used by the module chapters when knitting to pdf
badges <- list(
  markdownBadge = "https://img.shields.io/badge/Made%20with-Markdown-1f425f.png",
  genericBadge = "https://img.shields.io/badge/Get%20help-Report%20issues-%3CCOLOR%3E.png"
)
for (b in names(badges)) {
  f <- file.path(paths$figures, paste0(b, ".png"))
  if (!file.exists(f)) download.file(badges[[b]], destfile = f, mode = "wb")
}

# RENDER BOOK ------------------------------------------

## set manual version. From DESCRIPTION, by field name, so there is one source
## of truth and no per-release edit here to forget.
Sys.setenv(LANDR_MAN_VERSION = read.dcf("DESCRIPTION", fields = "Version")[1])

## don't use Require for package installation etc.
Sys.setenv(R_USE_REQUIRE = "false")

## NOTE: needs the dot, because knitting does `rm(list = ls())`
.copyModuleRmds <- prepManualRmds(modulePath = "./modules", rebuildCache = FALSE) ## use rel path!

## every format _output.yml declares: bs4_book, pdf_book (xelatex, krantz.cls,
## biblatex) and epub_book.
##
## bookdown knits once and then writes each format in turn, so the site is on
## disk before the PDF is attempted. Letting a LaTeX error propagate therefore
## threw away a site that had already rendered: the deploy step never ran and
## the published manual sat unchanged from 2026-09-21. The error is reported
## instead, as a GitHub Actions warning so it stays visible on the run, and the
## build carries on to deploy what did render. archiveManualPDF() below already
## does nothing when there is no PDF.
##
## The site itself must still not ship broken, so it is checked for separately.
## Removing it first means the check cannot pass on a stale local copy; in CI
## docs/ is absent anyway, as it is no longer committed.
indexHTML <- file.path(paths$docs, "index.html")
unlink(indexHTML)

tryCatch(
  bookdown::render_book(output_format = "all", envir = new.env()),
  error = function(e) {
    message("::warning title=Manual render incomplete::",
            "not every output format rendered: ", conditionMessage(e))
  }
)

if (!file.exists(indexHTML)) {
  stop("render_book() produced no '", indexHTML, "': the site itself failed to ",
       "render, so there is nothing to deploy.", call. = FALSE)
}

## .nojekyll and CNAME have to be inside the published directory: the deploy
## pushes the contents of docs/, so anything at the repository root never reaches
## the site. Without the CNAME, GitHub resets the custom domain on each deploy.
stagePagesFiles(paths$docs, cname = "landr-manual.predictiveecology.org")

archiveManualPDF(
  file.path(paths$docs, "LandRManual.pdf"),
  version = Sys.getenv("LANDR_MAN_VERSION"),
  prefix = "LandR-manual"
)

## Publish the archived release PDFs with the site. They are tracked under
## archive/pdf/ rather than left on gh-pages: that branch is rebuilt by every
## deploy, and an old PDF cannot be regenerated from current sources.
publishManualArchive(
  archiveDir = file.path(paths$prj, "archive", "pdf"),
  docsDir = paths$docs,
  manualName = "LandR Manual"
)

## remove the temporary .Rmds
unlink("_manual_rmds", recursive = TRUE)
