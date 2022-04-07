## -----------------------------------------------
## MANUAL KNITTING CONTROLLER SCRIPT
## -----------------------------------------------

## This manual must be knitted by running this script

## PACKAGES -----------------------------------------
## Make sure necessary packages are installed
if (!require("Require")) {
  install.packages("Require")
}

library(data.table)
Require::pkgSnapshot("packages/pkgSnapshot.txt")
# Much later on a different or same machine
# Require::Require(pkgSnapshot = "packages/pkgSnapshot.txt")

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
              "citations/referencesLandRManual.bib")
bibdata <- lapply(bibFiles, readLines)
write(unlist(bibdata), file = "citations/referencesLandRManual.bib")

if (!file.exists("citations/ecology-letters.csl")) {
  dir.create("citations", showWarnings = FALSE)
  download.file("https://www.zotero.org/styles/ecology-letters?source=1", destfile = "citations/ecology-letters.csl")
}

## BADGE IMAGES --------------------------------------
## add these to main figures/ folder so they can be used throughout module manuals when knitting to pdf
dir.create("figures", showWarnings = FALSE)

download.file(url = "https://img.shields.io/badge/Made%20with-Markdown-1f425f.png",
              destfile = "figures/markdownBadge.png",
              mode = 'wb')
download.file(url = "https://img.shields.io/badge/Get%20help-Report%20issues-%3CCOLOR%3E.png",
              destfile = "figures/genericBadge.png",
              mode = 'wb')

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

  ## make sure that setup chunk will be evaluated again
  ## (a previous setup chunk may have set "eval = FALSE" and "cache = TRUE")
  setupChunkStart <- which(grepl("```{r setup", linesModuleRmd, fixed = TRUE))
  setupChunkOptions <- linesModuleRmd[setupChunkStart]
  if (isFALSE(grepl("eval[[:space:]]*=[[:space:]]*TRUE", setupChunkOptions))) {
    setupChunkOptions <- if (grepl("eval", setupChunkOptions)) {
      sub("(.*)(eval[[:space:]]*=[[:space:]]*FALSE)(.*)\\}", "\\1eval = TRUE\\3\\}", setupChunkOptions)
    } else {
      sub("(.*)\\}", "\\1, eval = TRUE\\}", setupChunkOptions)
    }
  }

  if (isFALSE(grepl("cache[[:space:]]*=[[:space:]]*FALSE", setupChunkOptions))) {
    setupChunkOptions <- if (grepl("cache", setupChunkOptions)) {
      sub("(.*)(cache[[:space:]]*=[[:space:]]*)(TRUE|[[:digit:]])(.*)\\}", "\\1\\2FALSE\\4\\}", setupChunkOptions)
    } else {
      sub("(.*)\\}", "\\1, cache = FALSE\\}", setupChunkOptions)
    }
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

  ## if missing add chapter bibliography at the end of each module chapter:
  chapterBibLine <- grep("printbibliography|## References|# References", linesModuleRmd)
  needChapterBibLine <- TRUE

  if (length(chapterBibLine)) {
    ## if not in one of the last two lines, "move to the end"
    if (!chapterBibLine %in% c(length(linesModuleRmd), length(linesModuleRmd)-1)) {
      linesModuleRmd[chapterBibLine] <- NULL
    } else {
      needChapterBibLine <- FALSE
    }
  }

  if (needChapterBibLine) {
    linesModuleRmd <- capture.output(cat(linesModuleRmd, "\\printbibliography[segment=\\therefsegment,heading=subbibliography]", append = TRUE, sep = "\n"))
  }

  writeLines(linesModuleRmd, con = copyModuleRmd)

  return(copyModuleRmd)
})

## make sure there aren't repeated text references across modules
## first get module order
bkdwnYML <- readLines("_bookdown.yml")
bkdwnYMLsub <- bkdwnYML[grepl("modules\\/", bkdwnYML)]
bkdwnYMLsub <- sub(".*(modules)", "\\1", bkdwnYMLsub)

## now read all module lines and put the list in the right order
allModules <- lapply(copyModuleRmds, readLines)
names(allModules) <- copyModuleRmds
allModules <- allModules[bkdwnYMLsub]

## get the text ref lines and their line IDs
refTextLinesID <- lapply(allModules, function(x) {
  data.table(lineText = grep("^\\(ref\\:.*\\)", x, value = TRUE),
             lineID = grep("^\\(ref\\:.*\\)", x))
  })
refTextLinesID <- rbindlist(refTextLinesID, idcol = "file", use.names = TRUE)
refTextLinesID[, dups := duplicated(lineText)]

lapply(split(refTextLinesID, by = "file"), function(dupsTab, allModules) {
  if (any(dupsTab$dups)) {
    modLines <- allModules[[unique(dupsTab$file)]]
    modLines <- modLines[-dupsTab[which(dups), lineID]]

    ## if now we have two empty lines, remove one
    for (i in dupsTab[which(dups), lineID]) {
      if (all(modLines[c(i, i + 1)] == "")) {
        modLines <- modLines[-i]
      }
      if (all(modLines[c(i - 1, i)] == "")) {
        modLines <- modLines[-i]
      }
    }

    writeLines(modLines, con = unique(dupsTab$file))
  }
}, allModules = allModules)

## prevents GitHub from rendering book using Jekyll
if (!file.exists("docs/.nojekyll")) {
  file.create("docs/.nojekyll")
}

bookdown::render_book(output_format = "all")
# bookdown::render_book(output_format = "bookdown::pdf_book")

## remove temporary .Rmds
file.remove(copyModuleRmds)
