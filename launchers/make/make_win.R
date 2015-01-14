# commands to run Radiant
args <- commandArgs(trailingOnly = TRUE)
script_dir <- args[1]
class_app <- args[2:3]

# setting the url for the miniCRAN
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
pth <- normalizePath(paste0(script_dir,'/../../miniCRAN'),winslash='/')
# mcran <- paste0("file:///",pth)
# options(repos = c(CRAN = mcran))
# options(repos = "http://vnijs.rady.ucsd.edu/site_media/R")
options(repos = c(XRAN = "http://mostly-harmless.github.io/radiant_miniCRAN/"))

# install.packages('shiny', local_dir, dependencies = TRUE)
source(paste0(pth,"/pkgs.R"))

# udpate old-packages
update.packages(lib.loc = local_dir, ask = FALSE)

# install packages that are available but were not installed before
to_inp <- new.packages(lib.loc = local_dir)
if(length(to_inp) != 0)
	install.packages(to_inp, local_dir, dependencies = TRUE)

# install from scratch no matter if any packages are available
# install.packages(pkgs, local_dir, dependencies = TRUE)

com_string <- paste0("cd ",script_dir,"\n",Sys.which('R'), " -e ",
  "\"shiny::runApp('../../inst/", class_app[1],"', port = ", class_app[2],", launch.browser=TRUE)\"")
com_string

filename <- paste0(Sys.getenv("USERPROFILE"),"/Desktop/radiant_",class_app[1],".bat")
cat(com_string,file=filename,sep="\n")
