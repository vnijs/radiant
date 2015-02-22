# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# setting the url for the miniCRAN, using XRAN as in yihui/xran
options(repos = c(XRAN = "http://mostly-harmless.github.io/radiant_miniCRAN/"))

# udpate old-packages
update.packages(lib.loc = local_dir, ask = FALSE)

# install packages that are available but were not installed before
to_inp <- new.packages(lib.loc = local_dir)
if(length(to_inp) != 0)
	install.packages(to_inp, local_dir, dependencies = TRUE)

# load pkgs as defined in global.R - order is important!
if(exists("pkgs"))
	sapply(pkgs, require, lib.loc = local_dir, character.only=TRUE)

require(rpivotTable)

# options(repos = c(CRAN = "http://cran.rstudio.com"))
# install.packages('shiny')

# install.packages(devtools)
# devtools::install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
# shiny::runApp("~/gh/radiant_dev/inst/marketing")

# avoid .DS_Store files in git
# find . -name .DS_Store -print0 | xargs -0 git rm --ignore-unmatch
# http://stackoverflow.com/questions/18393498/gitignore-all-the-ds-store-files-in-every-folder-and-subfolder
