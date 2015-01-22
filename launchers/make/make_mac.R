class_app <- commandArgs(trailingOnly = TRUE)

# get the pkgs information from the repo on github
# source("http://mostly-harmless.github.io/radiant_miniCRAN/pkgs.R")

R_loc <- Sys.which('R')
if(R_loc == "") {
  cat("R is not installed or is not installed in the expected location")
  cat("\nMake sure R is installed on your system\n")
  Sys.sleep(60)
} else {
  cat("R was found in ",R_loc,"\n")
  script_dir <- getwd()

  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  # pth <- normalizePath(paste0(script_dir,'/../../launchers'),winslash='/')
  # mcran <- paste0("file:///",pth)
  # options(repos = c(CRAN = mcran))
  options(repos = c(XRAN = "http://mostly-harmless.github.io/radiant_miniCRAN/"))
  # options(repos = "http://mostly-harmless.github.io/radiant_miniCRAN/")
  # source(paste0(pth,"/pkgs.R"))

  # udpate old-packages
  update.packages(lib.loc = local_dir, ask = FALSE)

  # install packages that are available but were not installed before
  to_inp <- new.packages(lib.loc = local_dir)
  if(length(to_inp) != 0)
    install.packages(to_inp, local_dir, dependencies = TRUE)

  # install from scratch no matter if any packages are available
  # install.packages(pkgs, local_dir, dependencies = TRUE)

  app_dir <- normalizePath(paste0(script_dir,"/../../inst/",class_app[1],"/"))
  com_string <- paste0("#!/usr/bin/env Rscript\nshiny::runApp('",app_dir,"', port = ",class_app[2],", launch.browser=TRUE)")
  filename <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant_",class_app[1],".command")
  cat(com_string,file=filename,sep="\n")
  Sys.chmod(filename, mode = "0755")
}
