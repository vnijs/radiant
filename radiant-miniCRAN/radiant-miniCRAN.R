###############################################################
### RUN OUTSIDE OF RADIANT
###############################################################

# setting the default location to get R-packages
options(repos = c(CRAN = "http://cran.rstudio.com"))

# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# miniCRAN vignette
# http://cran.r-project.org/web/packages/miniCRAN/vignettes/miniCRAN.html
install.packages('miniCRAN', local_dir, dependencies = TRUE)

pth <- "~/Desktop/radiant_dev/radiant-miniCRAN"
if(!file.exists(pth)) dir.create(pth)
setwd(pth)
install.packages('./miniCRAN_0.1-0.tgz', local_dir, repos = NULL) # installing local package that fixes makeRepo from miniCRAN
require(miniCRAN)

# Specify list of packages to download
source('pkgs.R')

# building minicran for windows binaries
pkgList <- pkgDep(pkgs, type="source", suggests = FALSE, )
makeRepo(pkgList, path=pth, type="source")

# building minicran for windows binaries
pkgList <- pkgDep(pkgs, type="win.binary", suggests = FALSE, )
makeRepo(pkgList, path=pth, type="win.binary")

# building minicran for mac binaries
pkgList <- pkgDep(pkgs, type="mac.binary", suggests = FALSE, )
makeRepo(pkgList, path=pth, type="mac.binary")

# building minicran for mac binaries
pkgList <- pkgDep(pkgs, type="mac.binary.mavericks", suggests = FALSE, )
makeRepo(pkgList, path=pth, type="mac.binary.mavericks")
