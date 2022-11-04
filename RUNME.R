## -----------------------------------------------
## MANUAL KNITTING CONTROLLER SCRIPT
## -----------------------------------------------

## This manual must be knitted by running this script

## PACKAGES -----------------------------------------
## Make sure necessary packages are installed

## Sets up project library and renders book
options(repos = c(CRAN = "https://cloud.r-project.org"))

## note that "rmarkdown", "bookdown", "htmlwidgets" need to be installed in the default
## libraries with the same versions as in the project lib,
## because each .Rmd starts from a clean R session
if (!"rmarkdown" %in% installed.packages() ||
    packageVersion("rmarkdown") < "2.17") {
  install.packages("rmarkdown", dependencies = TRUE)
}

if (!"knitr" %in% installed.packages() ||
    packageVersion("knitr") < "1.40.4") {
  remotes::install_github("yihui/knitr", dependencies = TRUE)
}

if (!"bookdown" %in% installed.packages() ||
    packageVersion("bookdown") < "0.29") {
  install.packages("bookdown", dependencies = TRUE)
}

if (!"htmlwidgets" %in% installed.packages() ||
    packageVersion("htmlwidgets") < "1.5.4") {
  install.packages("htmlwidgets", dependencies = TRUE)
}

if (!"tinytex" %in% installed.packages() ||
    packageVersion("tinytex") < "0.41") {
  ## version "0.41" is probably not mandatory, but it's the version used as of Oct 27 2022
  tinytex::install_tinytex()
}

pkgPath <- normalizePath(file.path("packages", version$platform,
                                   paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                         winslash = "/")
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE)

if (!"remotes" %in% installed.packages(lib.loc = pkgPath))
  install.packages("remotes")

if (!"Require" %in% installed.packages(lib.loc = pkgPath) ||
   packageVersion("Require", lib.loc = pkgPath) < "0.1.6.9015") {
  remotes::install_github("PredictiveEcology/Require@bfb3ed19231d38362e7324f10435a387e29b6ce1",
                          upgrade = FALSE, force = TRUE)
}

## use binary linux packages if on Ubuntu
Require::setLinuxBinaryRepo()

pkgSnapshotFile <- file.path("packages",
                             paste0("pkgSnapshot_",
                                    paste0(version$major, "_", strsplit(version$minor, "[.]")[[1]][1]),
                                    ".txt"))
if (file.exists(pkgSnapshotFile)) {
  Require::Require(packageVersionFile = pkgSnapshotFile, standAlone = TRUE, upgrade = FALSE)
} else {
  Require::Require(c("bookdown", "ROpenSci/bibtex", "data.table", "downlit",
                     "formatR", "git2r", "kableExtra", "yihui/knitr",
                     "RefManageR", "rmarkdown", "pander", "openxlsx", "sylly", "xfun", "xml2",
                     "gdalUtils == 2.0.3.2", "RandomFieldsUtils", "RandomFields == 3.3.13",
                     "PredictiveEcology/LandR@d0df43e543abfeb0bca1c175b062288c10cb4dcb",
                     "PredictiveEcology/SpaDES@e8fb47e125fdace6bcbba2f7489a923f470fecf7",
                     "PredictiveEcology/SpaDES.docs@1a08beb7d53148e674d6bada7a7581483459cad2",
                     "PredictiveEcology/SpaDES.experiment@91bfad98d67ea2b7fcee3ea0115f8746e47534ad",
                     "PredictiveEcology/SpaDES.project@6d7de6ee12fc967c7c60de44f1aa3b04e6eeb5db"),
                   standAlone = TRUE, upgrade = FALSE, require = FALSE)

  if (!all(c("gdalUtils", "RandomFields") %in% rownames(installed.packages(lib.loc = pkgPath)))) {
    install.packages(c("https://cran.r-project.org/src/contrib/Archive/gdalUtils/gdalUtils_2.0.3.2.tar.gz",
                       "https://cran.r-project.org/src/contrib/Archive/RandomFields/RandomFields_3.3.13.tar.gz"),
                     type = "source", ## needed when repos = NULL (at least in Win OS)
                     repos = NULL)
  }
  ## save snapshot for later -- commented out against accidental overwrite
  # Require::pkgSnapshot(pkgSnapshotFile, libPaths = pkgPath, exact = TRUE, standAlone = TRUE)
}

## xfun failed to install from source using the specific version in pkgSnapshot
if (!"xfun" %in% rownames(installed.packages(lib.loc = pkgPath))) {
  install.packages("xfun", type = "source")
}

Require::Require(c("bookdown", "data.table", "RefManageR", "ROpenSci/bibtex"),
                 install = FALSE, upgrade = FALSE)

## REFERENCES ---------------------------------------
## automatically create a bib database for R packages
Require::checkPath("citations", create = TRUE)
write.bib(c(
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
Require::checkPath("figures", create = TRUE)

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
.copyModuleRmds <- SpaDES.docs::prepManualRmds("modules", rebuildCache = FALSE)

## RENDER BOOK ------------------------------------------
## prevents GitHub from rendering book using Jekyll
if (!file.exists("docs/.nojekyll")) {
  file.create("docs/.nojekyll")
}

## set manual version
Sys.setenv(LANDR_MAN_VERSION = "1.0.3") ## update this for each new release

## render the book using new env -- see <https://stackoverflow.com/a/46083308>
render_book(output_format = "all", envir = new.env())
# render_book(output_format = "bookdown::pdf_book", envir = new.env())
# render_book(output_format = "bookdown::bs4_book", envir = new.env())

overwriteArchivedPdf <- FALSE ## here to prevent accidental overwrite in case one forgets to update version.
pdfArchiveDir <- Require::checkPath(file.path("archive", "pdf"), create = TRUE)
file.copy(from = file.path("docs", "LandRManual.pdf"),
          to = file.path(pdfArchiveDir, paste0("LandR-manual-v", Sys.getenv("LANDR_MAN_VERSION"), ".pdf")),
          overwrite = overwriteArchivedPdf)

## remove temporary .Rmds
file.remove(.copyModuleRmds)

## to render each module manual separately (here for convenience)
moduleNames <- basename(list.dirs("modules", recursive = FALSE))
moduleRmds <- list.files("modules", pattern = paste0("(", paste(moduleNames, collapse = "|"),")\\.Rmd"),
                         recursive = TRUE, full.names = TRUE)
for (modRmd in moduleRmds[5]) {
  rmarkdown::render(modRmd, knit_root_dir = NULL)
}
