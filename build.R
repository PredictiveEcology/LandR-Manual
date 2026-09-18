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

## referencesLandRManual.bib holds the manual's own references -- those cited
## outside any module chapter -- and is both an input here and the output, since
## the manual accumulates into its own bibliography.
collapseModuleBibs(
  modulePath = file.path(paths$prj, "modules"),
  extraBibs = file.path(paths$citations,
                        c("references_SpaDES.bib", "packages.bib",
                          "referencesLandRManual.bib")),
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

## set manual version
Sys.setenv(LANDR_MAN_VERSION = "1.0.4") ## update this for each new release

## don't use Require for package installation etc.
Sys.setenv(R_USE_REQUIRE = "false")

## NOTE: needs the dot, because knitting does `rm(list = ls())`
.copyModuleRmds <- prepManualRmds(modulePath = "./modules", rebuildCache = FALSE) ## use rel path!

## HTML only for now. _output.yml also declares pdf_book (xelatex, krantz.cls,
## biblatex) and epub_book; `output_format = "all"` builds all three, so a
## missing LaTeX package fails the whole build. Restore "all" -- and the
## download links in _output.yml -- once the PDF toolchain is known to work.
bookdown::render_book(output_format = "bookdown::bs4_book", envir = new.env())

## .nojekyll and CNAME have to be inside the published directory: the deploy
## pushes the contents of docs/, so anything at the repository root never reaches
## the site. Without the CNAME, GitHub resets the custom domain on each deploy.
stagePagesFiles(paths$docs, cname = "landr-manual.predictiveecology.org")

archiveManualPDF(
  file.path(paths$docs, "LandRManual.pdf"),
  version = Sys.getenv("LANDR_MAN_VERSION"),
  prefix = "LandR-manual"
)

## remove the temporary .Rmds
unlink("_manual_rmds", recursive = TRUE)
