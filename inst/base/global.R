# for debugging
# options(warn=0)
# options(shiny.trace = TRUE)
# options(warn=2)
# options(shiny.error=recover)

pkgs_cran <- c("car", "gridExtra", "GPArotation", "psych", "wordcloud",
               "AlgDesign", "knitr", "lubridate", "ggplot2", "ggdendro",
               "shiny", "pryr", "shiny", "magrittr", "tidyr", "dplyr",
               "broom", "shinyAce")
pkgs <- pkgs_cran

if(Sys.getenv('SHINY_PORT') == "") {

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
  source("../base/dependencies.R", local = TRUE)
}

# adding the figures path to avoid making a copy of all figures in www/figures
if(!"package:radiant" %in% search()) {
  addResourcePath("figures", "../base/tools/help/figures/")
  addResourcePath("imgs", "../base/www/imgs/")
}
