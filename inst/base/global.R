# for debugging
# options(warn=0)
# options(shiny.trace = TRUE)
# options(warn=2)
# options(shiny.error=recover)

# load/attach packages
pkgs <- c("car", "gridExtra", "GPArotation", "psych", "vegan", "RColorBrewer",
          "wordcloud", "AlgDesign", "brew", "reshape2", "plyr", "markdown",
          "knitr", "rpivotTable", "rmarkdown", "testthat", "lubridate", "ggplot2", "pryr",
          "shiny", "magrittr", "tidyr", "dplyr", "ggvis", "broom", "shinyAce",
          "purrr", "rpivotTable")

if(Sys.getenv('SHINY_PORT') == "") {

  # running_local <<- TRUE
  running_local <- TRUE

  # no limit to filesize locally
  options(shiny.maxRequestSize=-1)

  # Windows or Mac
  if (.Platform$OS.type == 'windows') {
    Sys.setlocale(category = 'LC_ALL','English_United States.1252')
  } else {
    Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
  }

  # if radiant package was not loaded load dependencies
  if(!"package:radiant" %in% search())
    source("../base/dependencies.R", local = TRUE)

} else {

  running_local <- FALSE

  # limit upload filesize on server (5MB)
  options(shiny.maxRequestSize=5*1024^2)

  # load packages in order set by variable pkgs
  sapply(pkgs, require, lib.loc = Sys.getenv("R_LIBS_USER"),
         character.only=TRUE)
 }

# adding the figures path to avoid making a copy of all figures in www/figures
if(!"package:radiant" %in% search())
  addResourcePath("figures", "../base/tools/help/figures/")
  addResourcePath("imgs", "../base/www/imgs/")
