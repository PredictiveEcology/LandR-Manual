## -----------------------------------------------
## MANUAL KNITTING CONTROLLER SCRIPT
## -----------------------------------------------

## This manual must be knitted by running this script

## PACKAGES -----------------------------------------
## Make sure necessary packages are installed

if (!"remotes" %in% rownames(installed.packages())) {
  install.packages("remotes")
}

if (!require("Require")) {
  remotes::install_github("PredictiveEcology/Require@development")
  library("Require")
}

if (FALSE) {
  Require::pkgSnapshot("packages/pkgSnapshot.txt")
  ## Much later on a different or same machine
  #Require(packageVersionFile = "packages/pkgSnapshot.txt") ## TODO: doesn't work (Require#41)
  ## - uses absolute paths, uses machine-specific package location, uses R 4.0;
  ## - installing binaries of pkgs needed from source;
  ## - RandomFields and gdalUtils n/a on CRAN.
}

## BEGIN WORKAROUND for package installation

## install archived CRAN packages, which are N/A as April 2022
if (!all(c("gdalUtils", "RandomFields") %in% rownames(installed.packages()))) {
  install.packages("RandomFieldsUtils")
  install.packages(c("https://cran.r-project.org/src/contrib/Archive/gdalUtils/gdalUtils_2.0.3.2.tar.gz",
                     "https://cran.r-project.org/src/contrib/Archive/RandomFields/RandomFields_3.3.13.tar.gz"),
                   type = "source", ## needed when repos = NULL (at least in Win OS)
                   repos = NULL)
}

if (FALSE) {
  Require("PredictiveEcology/SpaDES.install@development")
  installSpatialPackages()
  installSpaDES()
}

## END WORKAROUND
Require(c("downlit", "formatR", "git2r", "rmarkdown", "xml2",
          "pander", "kableExtra", "yihui/knitr",
          "PredictiveEcology/SpaDES@development",
          "PredictiveEcology/SpaDES.docs@development",
          "PredictiveEcology/SpaDES.experiment@development",
          "PredictiveEcology/LandR@development"), require = FALSE)
Require(c("bookdown", "data.table",
          "RefManageR", "ROpenSci/bibtex"))

## REFERENCES ---------------------------------------
## automatically create a bib database for R packages
checkPath("citations", create = TRUE)
write_bib(c(
  .packages(), "bookdown", "knitr", "rmarkdown",
  "SpaDES.core", "SpaDES", "SpaDES.experiment", "reproducible",
  "LandR", "Require"
), "citations/packages.bib")

## collapse all chapter .bib files into one ------
bibFiles <- c(list.files("modules", "references_", recursive = TRUE, full.names = TRUE),
              "citations/packages.bib",
              "citations/referencesLandRManual.bib")
bibdata <- lapply(bibFiles, function(f) {
  if (file.exists(f)) ReadBib(f)
})
bibdata <- Reduce(merge, bibdata)

WriteBib(bibdata, file = "citations/referencesLandRManual.bib")

if (!file.exists("citations/ecology-letters.csl")) {
  download.file("https://www.zotero.org/styles/ecology-letters?source=1", destfile = "citations/ecology-letters.csl")
}

## BADGE IMAGES --------------------------------------
## add these to main figures/ folder so they can be used throughout module manuals when knitting to pdf
checkPath("figures", create = TRUE)

if (!file.exists("figures/markdownBadge.png")) {
  download.file(url = "https://img.shields.io/badge/Made%20with-Markdown-1f425f.png",
                destfile = "figures/markdownBadge.png",
                mode = "wb")
}

if (!file.exists("figures/genericBadge.png")) {
  download.file(url = "https://img.shields.io/badge/Get%20help-Report%20issues-%3CCOLOR%3E.png",
                destfile = "figures/genericBadge.png",
                mode = "wb")
}

## RMD PREP ------------------------------------------

## NOTE: need dot because knitting is doing `rm(list = ls())`
.copyModuleRmds <- SpaDES.docs::prepLandRRmds("modules", rebuildCache = FALSE)

## RENDER BOOK ------------------------------------------
## prevents GitHub from rendering book using Jekyll
if (!file.exists("docs/.nojekyll")) {
  file.create("docs/.nojekyll")
}

## set manual version
Sys.setenv(LANDR_MAN_VERSION = "1.0.1") ## update this for each new release

## render the book using new env -- see <https://stackoverflow.com/a/46083308>
render_book(output_format = "all", envir = new.env())
# render_book(output_format = "bookdown::pdf_book", envir = new.env())
# render_book(output_format = "bookdown::bs4_book", envir = new.env())

pdfArchiveDir <- checkPath(file.path("archive", "pdf"), create = TRUE)
file.copy(from = file.path("docs", "LandRManual.pdf"),
          to = file.path(pdfArchiveDir, paste0("LandR-manual-v", Sys.getenv("LANDR_MAN_VERSION"), ".pdf")),
          overwrite = TRUE)

## remove temporary .Rmds
file.remove(.copyModuleRmds)

