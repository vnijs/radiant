# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# setting the url for the miniCRAN, using XRAN as in yihui/xran :)
# options(repos = c(XRAN="http://vnijs.rady.ucsd.edu/site_media/R", CRAN="http://cran.rstudio.com"))
# options(repos = "http://vnijs.rady.ucsd.edu/site_media/R")
options(repos = c(XRAN = "http://mostly-harmless.github.io/radiant_miniCRAN/"))

# preferable to use online list BUT the import order is important
# (e.g., plyr and dplyr)
# pkgs <- available.packages() %>% rownames
# installed.packages() %>% rownames

# list of pkgs needed to run radiant
# pkgs <- c("car", "gridExtra", "GPArotation", "psych", "vegan",
#           "RColorBrewer", "wordcloud", "AlgDesign", "brew",
#           "reshape2", "plyr", "markdown", "knitr", "rmarkdown",
#           "testthat", "lubridate", "ggplot2", "shiny","magrittr",
#           "tidyr", "dplyr", "ggvis", "broom", "shinyAce")

# udpate old-packages
update.packages(lib.loc = local_dir, ask = FALSE)

# install packages that are available but were not installed before
to_inp <- new.packages(lib.loc = local_dir)
if(length(to_inp) != 0)
	install.packages(to_inp, local_dir, dependencies = TRUE)

# load/attach packages
# suppressWarnings(
#   sapply(pkgs, require, lib.loc = local_dir, character.only=TRUE)
# )
