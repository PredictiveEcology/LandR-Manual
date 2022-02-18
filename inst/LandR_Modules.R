moduleDependenciesToGraph <- function(md) {
  mods <- unique(c(md$from, md$to))
  m <- unlist(mods)
  v <- unique(c(md$to, md$from, m)) # so no need to remove them
  graph <- graph_from_data_frame(md, vertices = v, directed = TRUE)
}


PlotModuleGraph <- function(graph) {
  graph <- simplify(graph)

  names <- V(graph)$name
  groups <- ifelse(grepl("Biomass", names), "Biomass",
                   ifelse (grepl("fireSense", ignore.case = TRUE, names), "FireSense",
                           ifelse (grepl("CBM", ignore.case = TRUE, names), "CBM",
                                   ifelse (grepl("ROF", ignore.case = TRUE, names), "RoF", "Other"))))

  nodes <- data.frame(id = V(graph)$name, title = V(graph)$name, group = groups)
  nodes <- nodes[order(nodes$id, decreasing = F),]
  edges <- get.data.frame(graph, what="edges")[1:2]


  visNetwork(nodes, edges, width = "100%") %>%
    visIgraphLayout(layout = "layout_with_fr", type = "full") %>%
    visGroups(groupname = "Biomass", color = "orange",
              shadow = list(enabled = TRUE)) %>%
    # red triangle for group "B"
    visGroups(groupname = "FireSense", color = "red") %>%
    visGroups(groupname = "CBM", color = "green") %>%
    visGroups(groupname = "RoF", color = "lightgreen") %>%
    # visPhysics(repulsion = list(nodeDistance = 100)) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = TRUE,
               height="800px", width = "130%",
               #highlightNearest = list(enabled = T, degree = 1, hover = F),
               collapse = TRUE) %>%
    visInteraction(navigationButtons = TRUE)
}


moduleDepenencies <- function(modules, modulePath) {

  modsFlat <- unlist(modules)
  names(modsFlat) <- modsFlat
  obs <- lapply(modsFlat, function(mod) {
    io <- inputObjects(module = mod, path = modulePath)
    oo <- outputObjects(module = mod, path = modulePath)
    list(io = io[[mod]], oo = oo[[mod]], name = mod)
  })

  sim.in <- sim.out <- data.table(objectName = character(0),
                                  objectClass = character(0),
                                  module = character(0))

  lapply(obs, function(x) {
    if (!is.null(x)) {
      if (NROW(x$io)) {
        z.in <- as.data.table(x$io)[, .(objectName, objectClass)]
      } else {
        z.in <- data.table(objectName = character(), objectClass = character())
      }
      if (NROW(x$oo)) {
        z.out <- as.data.table(x$oo)[, .(objectName, objectClass)]
      } else {
        z.out <- data.table(objectName = character(), objectClass = character())
      }
      z.in$module <- z.out$module <- x$name
      if (!all(is.na(z.in[, objectName]), is.na(z.in[, objectClass]))) {
        sim.in <<- rbindlist(list(sim.in, z.in), use.names = TRUE)
      }
      if (!all(is.na(z.out[, 1:2]), is.na(z.out[, objectClass]))) {
        sim.out <<- rbindlist(list(sim.out, z.out), use.names = TRUE)
      }
    }
    return(invisible(NULL)) # return from the lapply
  })

  setkey(sim.in, "objectName")
  setkey(sim.out, "objectName")

  if ((nrow(sim.in)) && (nrow(sim.out))) {
    dx <- sim.out[sim.in, nomatch = NA_character_, allow.cartesian = TRUE]
    dx[is.na(module), module := "_INPUT_"]
    DT <- dx[, list(from = module, to = i.module,
                    objName = objectName, objClass = i.objectClass)]

  } else {
    DT <- data.table(from = character(0), to = character(0),
                     objName = character(0), objClass = character(0))
  }
  setorder(DT, "from", "to", "objName")
  DT <- DT[!grepl("INPUT", from)]
  return(DT)
}


validUrl <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}

listModules <- function(moduleGreps, accounts, ignore = c("fireSense_dataPrepFitRas", "fireSense_Size")) {
  names(accounts) <- accounts
  outs <- lapply(accounts, function(account) {
    url <- paste0("https://api.github.com/users/",account,"/repos?per_page=200")
    names(url) <- account

    tf <- tempfile()
    download.file(url, destfile = tf)
    suppressWarnings(repos <- readLines(tf))
    repos <- unlist(strsplit(repos, ","))

    out <- lapply(moduleGreps, function(mg) {
      if (grepl("PredictiveEcology", url) && mg == "scfm") browser()
      outs <- grep(mg, repos, value = TRUE)
      gitRepo <- grep("full_name", outs, value = TRUE)
      gitRepo <- strsplit(gitRepo, "\"")
      gitRepo <- grep(mg, unlist(gitRepo), value = TRUE)
      if (length(gitRepo)) {
        gitPaths <- paste0("https://github.com/",gitRepo,"/blob/master/",
                           basename(gitRepo),".Rmd")
        isRepo <- unlist(lapply(gitPaths, validUrl))
        if (any(!isRepo)) {
          notRepo <- gitRepo[!isRepo]
          message("This/these repo(s) is/are not a SpaDES module(s): ", notRepo)
        }
        gitRepo <- gitRepo[isRepo]
        outs <- grep("\"name", outs, value = TRUE)
        outs <- strsplit(outs, "\"")
        outs <- unlist(outs)
        outs <- grep(mg, outs, value = TRUE)
        outs <- intersect(basename(gitRepo), outs)
      } else {
        outs <- gitRepo
      }
      unique(outs)

    })
    grep(paste(ignore, collapse = "|"), unlist(out), invert = TRUE, value = TRUE)
  })
  outs
}
