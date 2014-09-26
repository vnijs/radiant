# install packages required to run radiant
# mcran <- paste0("file://",normalizePath("../../radiant-miniCRAN"))
if(file.exists("~/Desktop/radiant/")) {
  mcran <- paste0("file://",normalizePath("~/Desktop/radiant/radiant-miniCRAN"))
} else {
  mcran <- paste0("file://",normalizePath("~/Dropbox/radiant/radiant-miniCRAN"))
}
options(repos = c(CRAN = c(mcran,"http://cran.rstudio.com")))

# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# loading the list of pkgs needed to run radiant
# source("../../radiant-miniCRAN/pkgs.R")
source(paste0(mcran,"/pkgs.R"))

# check if all packages in libs are installed locally
available <- suppressWarnings(sapply(pkgs, require, lib.loc = c(local_dir), character.only=TRUE))
inst.libs <- pkgs[available == FALSE]
inst.libs

# install and require the packages that have not yet been installed
if(length(inst.libs) != 0) {
#   install.packages(inst.libs, local_dir, repos = mcran)
  install.packages(inst.libs, local_dir)
  suppressWarnings(sapply(inst.libs, require, lib.loc = c(local_dir),
  	character.only=TRUE))
}
