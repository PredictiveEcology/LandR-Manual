## This manual must be knitted by running this script.
##
## It replaces RUNME.R, which built a private library under
## packages/<platform>/<R-version> from snapshots taken for R 4.0 and 4.2, and
## installed `RandomFields` and `gdalUtils` from the CRAN archive. Neither builds
## on current R, and no module has referenced RandomFields for years. Package
## installation now happens in install.R, ahead of this script.

prjDir <- rprojroot::find_root(
  rprojroot::is_rstudio_project | rprojroot::is_git_root | rprojroot::from_wd,
  path = getwd()
)

docsDir <- file.path(prjDir, "_bookdown.yml") |>
  yaml::read_yaml() |>
  purrr::pluck("output_dir") |>
  fs::path_abs()

bibDir <- Require::checkPath(file.path(prjDir, "citations"), create = TRUE)

# load packages -------------------------------------

library(bibtex)
library(bookdown)
library(data.table)
library(knitr)
library(RefManageR)
library(SpaDES.docs)

## references ---------------------------------------

## a bib database for the R packages in use
allPkgs <- c("base", .packages(all.available = TRUE, lib.loc = .libPaths()))
suppressWarnings({
  write_bib(allPkgs, file.path(bibDir, "packages.bib"))
})

## index.Rmd cites the SpaDES package as @ChubatyMcIntire2019, but write_bib()
## keys package entries by package name (R-SpaDES), so that key resolves to
## nothing. Take the citation from the package itself rather than transcribing
## it, and file it under the key the prose uses. Nothing under citations/ was
## ever tracked, so the bib this key used to come from is not in the repository.
spadesEntry <- utils::toBibtex(utils::citation("SpaDES"))
spadesEntry[1] <- sub("\\{[^,]*,", "{ChubatyMcIntire2019,", spadesEntry[1])
writeLines(spadesEntry, file.path(bibDir, "references_SpaDES.bib"))

## collapse the chapter .bib files into one. referencesLandRManual.bib holds the
## manual's own references -- the ones cited outside any module chapter -- and is
## the only tracked file in citations/.
bibFiles <- c(
  list.files(file.path(prjDir, "modules"), "references_", recursive = TRUE, full.names = TRUE),
  file.path(bibDir, "references_SpaDES.bib"),
  file.path(bibDir, "packages.bib"),
  file.path(bibDir, "referencesLandRManual.bib")
)
bibdata <- lapply(bibFiles, function(f) {
  if (file.exists(f)) RefManageR::ReadBib(f)
})
bibdata <- Reduce(merge, bibdata)
WriteBib(bibdata, file = file.path(bibDir, "referencesLandRManual.bib"))

csl <- file.path(bibDir, "ecology-letters.csl")
if (!file.exists(csl)) {
  download.file("https://www.zotero.org/styles/ecology-letters?source=1", destfile = csl)
}

## badge images used by the module chapters when knitting to pdf
figDir <- Require::checkPath(file.path(prjDir, "figures"), create = TRUE)
badges <- list(
  markdownBadge = "https://img.shields.io/badge/Made%20with-Markdown-1f425f.png",
  genericBadge = "https://img.shields.io/badge/Get%20help-Report%20issues-%3CCOLOR%3E.png"
)
for (b in names(badges)) {
  f <- file.path(figDir, paste0(b, ".png"))
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

## Files the deployed site needs, which must sit *inside* the published folder:
## the deploy action pushes the contents of docs/ to gh-pages, so anything left
## at the repository root never reaches the site.
##   .nojekyll -- stops GitHub Pages running the output through Jekyll, which
##                would drop the `_`-prefixed directories bookdown emits
##   CNAME     -- the custom domain; without it GitHub resets the site to the
##                default *.github.io address on the next deploy
file.create(file.path(docsDir, ".nojekyll"))
writeLines("landr-manual.predictiveecology.org", file.path(docsDir, "CNAME"))

pdf <- file.path(docsDir, "LandRManual.pdf")
if (file.exists(pdf)) {
  pdfArchiveDir <- Require::checkPath(file.path(prjDir, "archive", "pdf"), create = TRUE)
  file.copy(
    from = pdf,
    to = file.path(pdfArchiveDir, paste0("LandR-manual-v", Sys.getenv("LANDR_MAN_VERSION"), ".pdf")),
    overwrite = TRUE
  )
}

## remove the temporary .Rmds
unlink("_manual_rmds", recursive = TRUE)
