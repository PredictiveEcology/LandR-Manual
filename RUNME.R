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
bibFiles <- c(list.files("modules", "references_", recursive = TRUE, full.names = TRUE),
              "citations/packages.bib",
              "citations/references_LandRManual.bib")
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

  ## make sure that setup chunk will be evaluated again (a previous setup chunk may have set "eval = FALSE")
  setupChunkStart <- which(grepl("```{r setup", linesModuleRmd, fixed = TRUE))
  setupChunkOptions <- linesModuleRmd[setupChunkStart]
  if (isFALSE(grepl("eval.*=TRUE", setupChunkOptions))) {
    setupChunkOptions <- sub("(.*)\\}", "\\1, eval = TRUE\\}", setupChunkOptions)
  }

  linesModuleRmd[setupChunkStart] <- setupChunkOptions

  ## change root.dir for each .Rmd
  existsRootDirsSetup <- any(grepl("root\\.dir", linesModuleRmd))   ## only searching for argument instead of <function>(.*<arg>) as the code may be split into different lines
  if (existsRootDirsSetup) {
    ## make sure the root.dir is the right one
    rootDirLine <- which(grepl("root\\.dir", linesModuleRmd))
    dir2replace <- normalizePath(dirname(copyModuleRmd), winslash = "/")
    code2replace <-  sub("(.*root\\.dir.*=[[:space:]]*)(.*)(\\))",
                         paste0("\\1", "'",  dir2replace, "'", "\\3"),
                         linesModuleRmd[rootDirLine])
    linesModuleRmd[rootDirLine] <- code2replace
  } else {
    ## break lines into 2 to add a working dir setup line
    beforeSetupChunkStart <- linesModuleRmd[1:setupChunkStart]
    afterSetupChunkStart <- linesModuleRmd[(setupChunkStart + 1):length(linesModuleRmd)]

    addedCode <- paste0("knitr::opts_knit$set(root.dir = '", normalizePath(dirname(copyModuleRmd), winslash = "/"), "')")

    linesModuleRmd <- c(beforeSetupChunkStart, addedCode, afterSetupChunkStart)
  }
  writeLines(linesModuleRmd, con = copyModuleRmd)

  return(copyModuleRmd)
})

bookdown::render_book()

## remove temporary .Rmds
file.remove(copyModuleRmds)
