# install packages required to run radiant
# next line checks if Radiant is being run locally or not

## Not sure how to use this for server

# setting the location of the miniCRAN
pth <- normalizePath(paste0("."))

# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# loading the list of pkgs needed to run radiant
source(paste0(pth,"/pkgs.R"))

# setting the url for the miniCRAN
mcran <- paste0("file:///",pth)

# look locally first and then in the Rstudion CRAN
# options(repos = c(CRAN = c(mcran,"http://cran.rstudio.com")))
options(repos = c(CRAN = mcran))

# udpate old-packages
update.packages(lib.loc = local_dir, ask = FALSE)

# install packages that are available but were not installed before
to_inp <- new.packages(lib.loc = local_dir)

#   if(length(to_inp) != 0) install.packages(to_inp, lib.loc = local_dir)
if(length(to_inp) != 0) install.packages(to_inp, local_dir)

# add function to remove packages that are not used by Radiant?
# might mess up someones stuff. maybe better to do this like packRat does it
# i.e., install into a non-standard directory

# load/attach packages
suppressWarnings(sapply(pkgs, require, lib.loc = local_dir, character.only=TRUE))
