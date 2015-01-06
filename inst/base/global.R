if(Sys.getenv('SHINY_PORT') == "") {
  # install and load required packages
  source("../../miniCRAN/dependencies.R", local = TRUE)

  # no limit to filesize locally
  options(shiny.maxRequestSize=-1)
  running_local <<- TRUE

  # vim key bindings for shinyAce
  vimKeyBinding <<- FALSE

  # if running on my computer
  if(file.exists("~/Desktop/GitHub/radiant_dev")) {
    # for debugging
    # options(shiny.trace = TRUE)
    # options(warn=0)
    # options(warn=2)
    # options(shiny.error=recover)
    # vimKeyBinding <<- TRUE
  }

} else {
  # load/attach packages, use dependencies-server.R to install relevant packages
  # before running shiny-server
  local_dir <- Sys.getenv("R_LIBS_USER")

  pkgs_cran <- c("car", "gridExtra", "GPArotation", "psych", "vegan",
                 "RColorBrewer", "wordcloud", "AlgDesign", "brew", "reshape2",
                 "plyr", "markdown", "knitr", "rmarkdown", "testthat",
                 "lubridate", "ggplot2", "shinyFiles", "shiny","magrittr",
                 "tidyr", "dplyr", "ggvis")
  pkgs_gh <- c("shinyAce", "rvest")
  pkgs <- c(pkgs_cran,pkgs_gh)

  suppressWarnings(
    sapply(pkgs, require, lib.loc = local_dir, character.only=TRUE)
  )

  # limit upload filesize on server (5MB)
  options(shiny.maxRequestSize=5*1024^2)
  running_local <<- FALSE
}

# Notation
# options(scipen = 100)
options(digits = 3)

# adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", "../base/tools/help/figures/")
addResourcePath("imgs", "../base/www/imgs/")
