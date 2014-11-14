# install packages required to run radiant
# setting the location of the miniCRAN
if(file.exists("radiant-miniCRAN")) {
  # This line is only here to deal with the old launchers on windows
  # Remove after Fall 2014
  pth <- normalizePath("./radiant-miniCRAN/", winslash = "/")
} else {
  pth <- normalizePath("../../radiant-miniCRAN", winslash = "/")
}

# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# setting the url for the miniCRAN
mcran <- paste0("file:///",pth)

# look locally first and then in the Rstudio CRAN
# options(repos = c(CRAN = c(mcran,"http://cran.rstudio.com")))
options(repos = c(CRAN = mcran)
source(paste0(pth,"/pkgs.R"))
install.packages(pkgs, local_dir, dependencies = TRUE)
