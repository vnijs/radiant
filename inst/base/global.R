# path to use for local and server use
r_path <- if(file.exists("../quant"))  ".." else system.file(package = "radiant")
if(r_path == "") r_path <- ".."  # if radiant is not installed revert to local inst

# r_path <- system.file(package = "radiant")

# reactive programming in Shiny requires (some) use of global variables
# currently these are r_env, r_data, r_state, r_local, and r_path

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

  r_local <- TRUE
  options(shiny.maxRequestSize=-1) # no limit to filesize locally

  # if radiant package was not loaded load dependencies
  if(!"package:radiant" %in% search())
    sapply(pkgs, require, character.only=TRUE)

} else {
  r_local <- FALSE
  options(shiny.maxRequestSize=5*1024^2) # limit upload filesize on server (5MB)
  sapply(pkgs, require, character.only=TRUE)
}

# adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", file.path(r_path,"/base/tools/help/figures/"))
addResourcePath("imgs", file.path(r_path,"/base/www/imgs/"))

### options used for debugging
# options(warn=0)
# options(warn=2)
# options(shiny.trace = TRUE)
# options(shiny.error=recover)

# Windows or Mac
# if (.Platform$OS.type == 'windows') {
#   Sys.setlocale(category = 'LC_ALL','English_United States.1252')
# } else {
#   Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
# }
