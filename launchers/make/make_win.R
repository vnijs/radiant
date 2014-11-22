# commands to run Radiant
args <- commandArgs(trailingOnly = TRUE)
script_dir <- args[1]
class_app <- args[2:3]

# setting the url for the miniCRAN
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
pth <- normalizePath(paste0(script_dir,'/../../radiant-miniCRAN'),winslash='/')
mcran <- paste0("file:///",pth)
options(repos = c(CRAN = mcran))

# install.packages('shiny', local_dir, dependencies = TRUE)
source(paste0(pth,"/pkgs.R"))
install.packages(pkgs, local_dir, dependencies = TRUE)

com_string <- paste0("cd ",script_dir,"\n",Sys.which('R'), " -e ",
  "\"shiny::runApp('../../inst/", class_app[1],"', port = ", class_app[2],", launch.browser=TRUE)\"")
com_string

filename <- paste0(Sys.getenv("USERPROFILE"),"/Desktop/radiant_",class_app[1],".bat")
cat(com_string,file=filename,sep="\n")
