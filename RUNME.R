## -----------------------------------------------
## MANUAL KNITTING CONTROLLER SCRIPT
## -----------------------------------------------

## This manual must be knitted by running this script

## PACKAGES -----------------------------------------
## Make sure necessary packages are installed
if(!require("Require")) {
  install.packages("Require")
}

# Require::pkgSnapshot("packages/pkgSnapshot.txt")
# Much later on a different or same machine
Require::Require(pkgSnapshot = "packages/pkgSnapshot.txt")

## REFERENCES ---------------------------------------
## automatically create a bib database for R packages
knitr::write_bib(c(
  .packages(), "bookdown", "knitr", "rmarkdown",
  "SpaDES.core", "SpaDES", "SpaDES.experiment", "reproducible",
  "LandR", "Require"
), "citations/packages.bib")

## collapse all chapter .bib files into one ------
## NOT WORKING!!!
bibFiles <- c(list.files("modules", "references_", recursive = TRUE, full.names = TRUE),
              "citations/packages.bib")
bibdata <- lapply(bibFiles, readLines)
write(unlist(bibdata), file = "citations/references_LandRManual.bib")

## RMD PREP ------------------------------------------
## strip module.Rmd YAML headers -----
moduleRmds <- list.dirs("modules", recursive = FALSE)
moduleRmds <- paste0(file.path(moduleRmds, basename(moduleRmds)), ".Rmd")

copyModuleRmds <- sapply(moduleRmds, function(x) {
  copyModuleRmd <- sub("(.*)(\\.Rmd)", "\\12\\2", x)
  file.copy(x, copyModuleRmd, overwrite = TRUE)

  linesModuleRmd <- readLines(copyModuleRmd)
  lines2Rm <- modelr::seq_range(which(linesModuleRmd == "---"), by = 1)
  linesModuleRmd <- linesModuleRmd[-lines2Rm]

  writeLines(linesModuleRmd, con = copyModuleRmd)
  copyModuleRmd
})

bookdown::render_book()

## remove temporary .Rmds
file.remove(copyModuleRmds)
