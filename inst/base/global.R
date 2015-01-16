# load/attach packages
pkgs <- c("car", "gridExtra", "GPArotation", "psych", "vegan", "RColorBrewer",
          "wordcloud", "AlgDesign", "brew", "reshape2", "plyr", "markdown",
          "knitr", "rmarkdown", "testthat", "lubridate", "ggplot2", "shiny",
          "magrittr", "tidyr", "dplyr", "ggvis", "broom", "shinyAce")

# vim key bindings for shinyAce ... which don't work well
vimKeyBinding <<- FALSE

if(Sys.getenv('SHINY_PORT') == "") {

  running_local <<- TRUE

  # no limit to filesize locally
  options(shiny.maxRequestSize=-1)

  # Windows or Mac
  if (.Platform$OS.type == 'windows') {
    Sys.setlocale(category = 'LC_ALL','English_United States.1252')
  } else {
    Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
  }

  # install and load required packages
  source("../base/dependencies.R", local = TRUE)

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

  running_local <<- FALSE

  # limit upload filesize on server (5MB)
  options(shiny.maxRequestSize=5*1024^2)

  suppressWarnings(
      sapply(pkgs, require, lib.loc = Sys.getenv("R_LIBS_USER"),
             character.only=TRUE)
  )
 }

# Notation
# options(scipen = 100)
options(digits = 3)

# adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", "../base/tools/help/figures/")
addResourcePath("imgs", "../base/www/imgs/")
