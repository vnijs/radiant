# path to use for local and server use
# path <- ".."

# path to use for shinyapps.io
path <- system.file(package = "radiant")

# if radiant is not installed revert to path in the inst directory
if(path == "") path <- ".."

### options used for debugging
# options(warn=0)
# options(warn=2)
# options(shiny.trace = TRUE)
# options(shiny.error=recover)

options("width"=200)
options("scipen"=100, "digits"=4)

pkgs_cran <- c("car", "gridExtra", "GPArotation", "psych", "wordcloud",
               "AlgDesign", "knitr", "lubridate", "ggplot2", "ggdendro",
               "shiny", "pryr", "shiny", "magrittr", "tidyr", "dplyr",
               "broom", "htmlwidgets")
pkgs_gh <- c("shinyAce","rpivotTable","DT")
pkgs <- c(pkgs_cran, pkgs_gh)

# list of function arguments
expl_functions <- list("n" = "length", "mean" = "mean_rm", "median" = "median_rm",
                       "min" = "min_rm", "max" = "max_rm", "25%" = "p25",
                       "75%" = "p75", "sd" = "sd_rm", "se" = "serr",
                       "cv" = "cv", "skew" = "skew", "kurtosis" = "kurtosi",
                       "# missing" = "nmissing")

# environment to hold session information
sessionStore <- new.env(parent = emptyenv())

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
    source(file.path(path, "/base/dependencies.R"), local = TRUE)

} else {
  running_local <- FALSE
  # limit upload filesize on server (5MB)
  options(shiny.maxRequestSize=5*1024^2)
  # source("../base/dependencies.R", local = TRUE)
  # source(file.path(path, "/base/dependencies.R"), local = TRUE)
  # source("for_shinyapps.R", local = TRUE)
  if(exists("pkgs"))
    sapply(pkgs, require, character.only=TRUE)
}

# adding the figures path to avoid making a copy of all figures in www/figures
if(!"package:radiant" %in% search()) {
  addResourcePath("figures", file.path(path,"/base/tools/help/figures/"))
  addResourcePath("imgs", file.path(path,"/base/www/imgs/"))
}
