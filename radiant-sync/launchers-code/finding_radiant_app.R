# options(echo = FALSE)
# options(echo = TRUE)
setwd("~")

if(file.exists("~/Dropbox")) {
  pth <- normalizePath("~/Dropbox", winslash = "/")
  cat(paste("Dropbox found in", pth,"\n"))
} else if(file.exists("~/../Dropbox")) {
  pth <- normalizePath("~/../Dropbox", winslash = "/")
  cat(paste("Dropbox found in", pth,"\n"))
} else {
  cat("No dropbox location found. Did you install the Dropbox software from https://www.dropbox.com/downloading ?\n")
  q("ask")
}

setwd(pth)

if(file.exists("radiant")) {
  pth <- normalizePath("radiant", winslash = "/")
  cat(paste("radiant folder Dropbox found in", pth,"\n"))
} else {
  cat("No radiant folder found in your Dropbox. \nDid you request access by sending an email to radiant@rady.ucsd.edu? \nMake sure to use the email address that is registered with Dropbox.\n")
  q("ask")
}

setwd(pth)

source("radiant-miniCRAN/dependencies.R")

if (.Platform$OS.type == 'windows') {
  Sys.setlocale(category = "LC_ALL","English_United States.1252")
} else {
  Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
}

shiny::runApp('inst/base', port = 4000, launch.browser=TRUE)
# shiny::runApp('inst/quant', port = 4403, launch.browser=TRUE)
# shiny::runApp('inst/marketing', port = 4475, launch.browser=TRUE)
