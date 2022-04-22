#library(rsconnect)
#library(knitr)
Require::Require("rmarkdown")
setwd("inst")
source("LandR_Modules.R")
rmarkdown::render(input = "LandR_Modules.Rmd", output_file="LandR_Modules.html")
# result <- markdown::rpubsUpload("LandR module ecosystem", "LandR_Modules.html")
# if (!is.null(result$continueUrl))
#   browseURL(result$continueUrl)
# else
#   stop(result$error)

# update the same document with a new title
updateResult <- markdown::rpubsUpload(
  "LandR module ecosystem", "LandR_Modules.html",
  id = "https://api.rpubs.com/api/v1/document/867563/e0232cc23d03426095251b77312fa626")


#> result
#$id
#[1] "https://api.rpubs.com/api/v1/document/867563/e0232cc23d03426095251b77312fa626"
#
#$continueUrl
#[1] "http://rpubs.com/publish/claim/867563/d244fc1f7e0344e7b44207a3e2bcf5f5"
