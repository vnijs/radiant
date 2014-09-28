# install packages required to run radiant
if(Sys.getenv('SHINY_PORT') == "") {

  if(file.exists("~/Desktop/radiant_dev/")) {
    mcran <- paste0("file:///",normalizePath("~/Desktop/radiant_dev/radiant-miniCRAN", winslash = "/"))
  } else {
    if(file.exists('~/Dropbox/radiant')) {
      mcran <- paste0("file:///",normalizePath("~/Dropbox/radiant/radiant-miniCRAN", winslash = "/"))
    } else {
      mcran <- paste0("file:///",normalizePath("~/../Dropbox/radiant/radiant-miniCRAN", winslash = "/"))
    }
  }
} else {
  # if this is not being run locally ...
#   mcran <- paste0("file:///",normalizePath("~/Desktop/radiant/radiant-miniCRAN", winslash = "/"))
  mcran <- "file:///home/vnijs/Desktop/radiant/radiant-miniCRAN"
}

# look locally first and then in the Rstudion CRAN
# options(repos = c(CRAN = c(mcran,"http://cran.rstudio.com")))
options(repos = c(CRAN = mcran))

# install to user directory
if(Sys.getenv('SHINY_PORT') == "") {
  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# loading the list of pkgs needed to run radiant
# source("../../radiant-miniCRAN/pkgs.R")
source(paste0(mcran,"/pkgs.R"))

# check if all packages in libs are installed locally
available <- suppressWarnings(sapply(pkgs, require, lib.loc = local_dir, character.only=TRUE))
inst.libs <- pkgs[available == FALSE]

# install and require the packages that have not yet been installed
if(length(inst.libs) != 0) {
  install.packages(inst.libs, local_dir)
  suppressWarnings(sapply(inst.libs, require, lib.loc = local_dir, character.only=TRUE))
}

} else {
  options(repos = c(CRAN = c("http://cran.rstudio.com")))
  source(paste0(mcran,"/pkgs.R"))
  install.packages(pkgs)
  suppressWarnings(sapply(pkgs, require, character.only=TRUE))
}
