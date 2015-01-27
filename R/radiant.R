#' Launch the Radiant app in a browser
#'
#' @details See \url{http://mostly-harmless.github.io/radiant} for documentation and tutorials
#'
#' @param app Choose the app to run. Either "base", "quant", or "marketing". "marketing" is the default
#'
#' @export
radiant <- function(app = c("marketing", "quant", "base")) {

  if(packageVersion('shiny') > "0.10.2.2")
    warning("Radiant currently does not support Shiny 0.11. To use Radiant please remove Shiny 0.11.\nCopy-and-paste the code below to install the required packages.\n\n\ndetach('package:shiny', unload=TRUE)\nremove.packages('shiny')\noptions(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/'))\ninstall.packages(new.packages(), dependencies = TRUE)")

  addResourcePath("imgs", system.file("base/www/imgs/", package="radiant"))
  addResourcePath("figures", system.file("base/tools/help/figures/", package="radiant"))
  if(app[1] == "marketing") {
    addResourcePath("figures_marketing", system.file("marketing/tools/help/figures/", package="radiant"))
  } else if(app[1] == "quant") {
    addResourcePath("figures_quant", system.file("quant/tools/help/figures/", package="radiant"))
  }

  runApp(system.file(app[1], package='radiant'))
}

#' Add "***" to a data.frame based on the p.values
#'
#' @param p.value Vector of p.values from an analysis
#'
#' @return A list with all variables defined in the function
#'
#' @examples
#' sig_stars(c(.0009,.009, .049, .09, .4))
#'
#' @export
sig_stars <- function(p.value) {
  sapply(p.value, function(x) x < c(.001,.01, .05, .1)) %>%
  colSums %>%
  add(1) %>%
  c("",".","*", "**", "***")[.]
}

#' Avoid warnings and messages. Adapted from http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/
#'
#' @examples
#' sshh( library(dplyr) )
#'
#' @export
sshh <- function(...) {
  suppressWarnings( suppressMessages( ... ) )
  invisible()
}

#### test
# sshh(library(dplyr))
# detach("package:dplyr")
# library(dplyr)
#### end test


# probably not added to global environment because the environment is
# attached inside a function
# if(exists("env_shiny")) attach(env_shiny)
# need to detach if used
# if(exists("env_shiny")) detach(env_shiny)

#' Get data for analysis functions exported by Radiant
#'
#' @param dataset Name of the data.frame
#' @param vars Variables to extract from the data.frame
#' @param na.rm Remove rows with missing values (default is TRUE)
#'
#' @return Data.frame with the specified columns selected
#'
#' @export
getdata_exp <- function(dataset, vars, na.rm = TRUE) {

  #########################
  # specify search order?
  #########################
  clean <- ifelse(na.rm, na.omit, f(...))
  if(exists(dataset)) {
    cat("Data loaded from global environment\n")
    select_(get(dataset), .dots = vars) %>% clean
  } else if(exists("values")) {
    if(values[[dataset]] %>% is.null) {
      paste0("Dataset ", dataset, " is not available. Please load the dataset and put the name in the function call") %>%
        stop %>% return
    }
    cat("Data loaded from 'values' list\n")
    select_(values[[dataset]], .dots = vars) %>% clean
  } else if(exists("env_shiny") && exists("values", envir = env_shiny)) {
    cat("Data loaded from env_shiny\n")
    select_(get("values", envir = env_shiny)[[dataset]], .dots = vars) %>% clean
  } else {
    paste0("Dataset ", dataset, " is not available. Please load the dataset and put the name in the function call") %>%
      stop %>% return
  }
}

# test
# library(dplyr)
# values <- list()
# values$mtcars <- mtcars
# getdata_exp("mtcars", c("cyl","mpg"))
# values$mtcars[5:20,2] <- NA
# getdata_exp("mtcars", c("cyl","mpg"))
# env_shiny <- new.env()
# env_shiny$values <- values
# rm(values)
# getdata_exp("mtcars", c("cyl","mpg"))
# rm(env_shiny)
# getdata_exp("mtcars", c("cyl","mpg"))
# getdata_exp("mtcarsx", c("cyl","mpg"))
# end test


# @import car gridExtra GPArotation psych vegan RColorBrewer wordcloud AlgDesign brew reshape2 plyr markdown knitr rmarkdown testthat lubridate ggplot2 shiny magrittr tidyr dplyr ggvis broom shinyAce
# @importFrom shiny addResourcePath runApp
