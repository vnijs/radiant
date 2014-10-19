class_app <- commandArgs(trailingOnly = TRUE)

R_loc <- Sys.which('R')
if(R_loc == "") {
  cat("R is not installed or is not installed in the expected location")
  cat("\nMake sure R is installed on your system\n")
  Sys.sleep(60)
} else {
  cat("R was found in ",R_loc,"\n")
  script_dir <- getwd()

  if(!'shiny' %in% installed.packages()[,'Package']) {
    # setting the url for the miniCRAN
    local_dir <- Sys.getenv("R_LIBS_USER")
    if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
    pth <- normalizePath("../../radiant-miniCRAN")
    mcran <- paste0("file:///",pth)
    options(repos = c(CRAN = mcran))
    install.packages('shiny', local_dir)
  }

  app_dir <- normalizePath(paste0(script_dir,"/../../inst/",class_app[1],"/"))
  com_string <- paste0("#!/usr/bin/env Rscript\nshiny::runApp('",app_dir,"', port = ",class_app[2],", launch.browser=TRUE)")
  filename <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant_",class_app[1],".command")
  cat(com_string,file=filename,sep="\n")
  Sys.chmod(filename, mode = "0755")
}
