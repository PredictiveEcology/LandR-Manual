## --------------------------------------------------
## LandR-Manual miscellaneous functions
## --------------------------------------------------

#' Prepare module .Rmd for book rending
#'
#' Creates modified versions of the modules' .Rmd files,
#'   with YAML headers removed and adapted knitr setup
#'   chunks
#'
#' @param modulePath modules' folder directory
#'
#' @return directories for modified module .Rmd files
#'
#' @importFrom Require normPath
#' @importFrom modelr seq_range

.prepRmds <- function(modulePath, rebuildCache = FALSE) {
  moduleRmds <- list.dirs(modulePath, recursive = FALSE)
  moduleRmds <- paste0(file.path(moduleRmds, basename(moduleRmds)), ".Rmd")

  copyModuleRmds <- sapply(moduleRmds, rebuildCache = rebuildCache,
                           FUN = function(x, rebuildCache) {
    copyModuleRmd <- sub("(.*)(\\.Rmd)", "\\12\\2", x)
    file.copy(x, copyModuleRmd, overwrite = TRUE)

    ## strip module.Rmd YAML headers -----
    linesModuleRmd <- readLines(copyModuleRmd)
    lines2Rm <- modelr::seq_range(which(linesModuleRmd == "---"), by = 1)
    linesModuleRmd <- linesModuleRmd[-lines2Rm]

    ## add chapter title if not present
    nonEmptyLines <- linesModuleRmd[linesModuleRmd != ""]
    if (!grepl("^# ", nonEmptyLines[1])) {
      modName <- sub(".Rmd", "" ,basename(x))
      chapterTitle <- paste0("# LandR *", modName, "* Module")

      linesModuleRmd <- c("", chapterTitle, linesModuleRmd)
    }

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
      dir2replace <- normPath(dirname(copyModuleRmd))
      code2replace <-  sub("(.*root\\.dir.*=[[:space:]]*)(.*)(\\))",
                           paste0("\\1", "'",  dir2replace, "'", "\\3"),
                           linesModuleRmd[rootDirLine])
      linesModuleRmd[rootDirLine] <- code2replace
    } else {
      ## break lines into 2 to add a working dir setup line
      beforeSetupChunkStart <- linesModuleRmd[1:setupChunkStart]
      afterSetupChunkStart <- linesModuleRmd[(setupChunkStart + 1):length(linesModuleRmd)]

      addedCode <- paste0("knitr::opts_knit$set(root.dir = '", normPath(dirname(copyModuleRmd)), "')")

      linesModuleRmd <- c(beforeSetupChunkStart, addedCode, afterSetupChunkStart)
    }

    ## add cache rebuild options for each .Rmd
    existsCacheRebuildSetup <- any(grepl("cache.rebuild", linesModuleRmd))
    if (existsCacheRebuildSetup) {
      ## overwrite option
      cacheRebuildLine <- which(grepl(",*[[:space:]]*cache.rebuild[[:space:]]*=[[:space:]]*(TRUE|FALSE)[[:space:]]*(,|\\})", linesModuleRmd))
      code2replace <- sub("(.*cache\\.rebuild.*=[[:space:]]*)(TRUE|FALSE)(.*)",
                          paste0("\\1", rebuildCache, "\\3"),
                          linesModuleRmd[cacheRebuildLine])
      linesModuleRmd[cacheRebuildLine] <- code2replace
    } else {
      ## break lines into 2 to add a cache rebuild dir setup line (it doesn't matter if there
      ## is another call to `knitr::opts_chunk$set`)
      beforeSetupChunkStart <- linesModuleRmd[1:setupChunkStart]
      afterSetupChunkStart <- linesModuleRmd[(setupChunkStart + 1):length(linesModuleRmd)]

      addedCode <- paste0("knitr::opts_chunk$set(cache.rebuild = ", rebuildCache, ")")

      linesModuleRmd <- c(beforeSetupChunkStart, addedCode, afterSetupChunkStart)
    }

    ## if missing add chapter bibliography at the end of each module chapter:
    chapterBibLine <- grep("printbibliography|## References|# References", linesModuleRmd)
    needChapterBibLine <- TRUE

    if (length(chapterBibLine)) {
      ## if not in one of the last two lines, "move to the end"
      if (!chapterBibLine %in% c(length(linesModuleRmd), length(linesModuleRmd) - 1)) {
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

  return(copyModuleRmds)
}
