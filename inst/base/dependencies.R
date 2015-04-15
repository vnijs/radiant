# Used to install dependencies on shiny-server (not for shinyapp.io)
# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# is a repos set?
repos <- options()$repos
if (is.null(repos)) repos <- "http://cran.r-project.org"

# setting the url for the miniCRAN, as in yihui/xran
options(repos = "http://vnijs.github.io/radiant_miniCRAN/")

# update old-packages
update.packages(lib.loc = local_dir, ask = FALSE)

# install packages that are available but were not installed before
to_inp <- new.packages(lib.loc = local_dir)
if (length(to_inp) != 0)
	install.packages(to_inp, local_dir, dependencies = TRUE)

# load pkgs as defined in global.R - order is important (e.g., plyr vs dplyr)
# so you have to use pkgs, sourced either locally or remotely
if (exists("pkgs"))
	sapply(pkgs, require, lib.loc = local_dir, character.only=TRUE)

# set repos to previous value
options(repos = repos)
