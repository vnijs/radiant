#' Launch Radiant
#'
#' @importFrom shiny addResourcePath runApp
#'
#' @export
#'
radiant <- function(app = c("marketing","quant")) {

  if(packageVersion('shiny') > "0.10.2.2")
    stop("Radiant currently does not support Shiny 0.11. To use Radiant please remove Shiny 0.11.\nCopy-and-paste the code below to install the required packages.\n\n\ndetach('package:shiny', unload=TRUE)\nremove.packages('shiny')\noptions(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/'))\ninstall.packages(new.packages(), dependencies = TRUE)")

  addResourcePath("figures", system.file("base/tools/help/figures/", package="radiant"))
  addResourcePath("imgs", system.file("base/www/imgs/", package="radiant"))
  if(app == "marketing")
    addResourcePath("figures_marketing", system.file("marketing/tools/help/figures/", package="radiant"))
  if(app == "quant")
    addResourcePath("figures_quant", system.file("quant/tools/help/figures/", package="radiant"))

  runApp(system.file("marketing", package='radiant'))
}

# # load/attach packages
# pkgs <- c("car", "gridExtra", "GPArotation", "psych", "vegan", "RColorBrewer",
#           "wordcloud", "AlgDesign", "brew", "reshape2", "plyr", "markdown",
#           "knitr", "rmarkdown", "testthat", "lubridate", "ggplot2", "shiny",
#           "magrittr", "tidyr", "dplyr", "ggvis", "broom", "shinyAce")

# # vim key bindings for shinyAce ... which don't work well
# vimKeyBinding <<- FALSE

# if(Sys.getenv('SHINY_PORT') == "") {

#   running_local <<- TRUE

#   # no limit to filesize locally
#   options(shiny.maxRequestSize=-1)

#   # Windows or Mac
#   if (.Platform$OS.type == 'windows') {
#     Sys.setlocale(category = 'LC_ALL','English_United States.1252')
#   } else {
#     Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
#   }

#   # install and load required packages
#   source("../base/dependencies.R", local = TRUE)

#   if (Sys.getenv("HOME") == "/Users/vnijs") {
#     # for debugging
#     # options(shiny.trace = TRUE)
#     # options(warn=0)
#     # options(warn=2)
#     # options(shiny.error=recover)
#     # vimKeyBinding <<- TRUE
#   }
# } else {

#   running_local <<- FALSE

#   # limit upload filesize on server (5MB)
#   options(shiny.maxRequestSize=5*1024^2)

#   # load packages in order set by variable pkgs
#   suppressWarnings(
#       sapply(pkgs, require, lib.loc = Sys.getenv("R_LIBS_USER"),
#              character.only=TRUE)
#   )
#  }
