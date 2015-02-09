#' Launch the Radiant app in a browser
#'
#' @details See \url{http://mostly-harmless.github.io/radiant} for documentation and tutorials
#'
#' @param app Choose the app to run. Either "base", "quant", or "marketing". "marketing" is the default
#'
#' @export
radiant <- function(app = c("marketing", "quant", "base")) {

  addResourcePath("imgs", system.file("base/www/imgs/", package="radiant"))
  addResourcePath("figures", system.file("base/tools/help/figures/", package="radiant"))
  if(app[1] == "marketing") {
    addResourcePath("figures_marketing", system.file("marketing/tools/help/figures/", package="radiant"))
  } else if(app[1] == "quant") {
    addResourcePath("figures_quant", system.file("quant/tools/help/figures/", package="radiant"))
  }

  runApp(system.file(app[1], package='radiant'))
}

#' Alias to set the class for an object. In radiant this is organized per analysis function (e.g., single_mean)
#'
#' @examples
#' foo <- function(x) x^2 %>% set_class(c("foo", class(.)))
#'
#' @export
set_class <- `class<-`

#' Add '***' to a data.frame based on the p.values
#'
#' @param p.value Vector of p.values from an analysis
#'
#' @return A list with all variables defined in the function
#'
#' @examples
#' sig_stars(c(.0009, .049, .009, .4, .09))
#'
#' @export
sig_stars <- function(pval) {
  sapply(pval, function(x) x < c(.001,.01, .05, .1)) %>%
    colSums %>% add(1) %>%
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

#' Get data for analysis functions exported by Radiant
#'
#' @param dataset Name of the data.frame
#' @param vars Variables to extract from the data.frame
#' @param na.rm Remove rows with missing values (default is TRUE)
#' @param filt Filter to apply to the data (default is NULL)
#'
#' @return Data.frame with the specified columns selected
#'
#' @export

getdata_exp <- function(dataset, vars, na.rm = TRUE, filt = "") {

  # dat <- mtcars
  # filt <- "mpg > 20\n"
  # filt %<>% gsub("\\s","", .)
  # dat %>% { if(filt == "") . else filter_(.,filt) }
  # filt <- ""
  # dat %>% { if(filt == "") . else filter_(.,filt) }
  # dat[1,1] <- NA
  # na.rm <- FALSE
  # dat %>% { if(na.rm) na.omit(.) else .}
  # na.rm <- TRUE
  # dat %>% { if(na.rm) na.omit(.) else .}

  filt %<>% gsub("\\s","", .)

  if(exists("session") && exists("r_env")) {
    # cat("Dataset", dataset, "loaded from the radiant environment (r_env)\n")
    select_(getdata(), .dots = vars) %>%
      { if(na.rm) na.omit(.) else . }
  } else if(exists("r_env")) {
    cat("The requested dataset cannot be loaded from r_env. To make data from Radiant accesible in R(studio), stop the application using Quit > Quit in the navigation bar.\n") %>%
      stop %>% return
  } else if(exists("r_data") && !is.null(r_data[[dataset]])) {
    if(running_local) cat("Dataset", dataset, "loaded from r_data list\n")
    select_(r_data[[dataset]], .dots = vars) %>%
      { if(na.rm) na.omit(.) else . } %>%
      { if(filt == "") . else filter_(.,filt) }
  } else if(exists(dataset)) {
    cat("Dataset", dataset, "loaded from global environment\n")
    select_(get(dataset), .dots = vars) %>%
      { if(na.rm) na.omit(.) else . } %>%
      { if(filt == "") . else filter_(.,filt) }
  } else {
    paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
      stop %>% return
  }
}

# test
# rm(list = ls())
# library(dplyr)
# library(magrittr)
# running_local <- TRUE
# mtcars_ <- mtcars
# getdata_exp("mtcars_", c("cyl","mpg"))
# r_data <- list()
# r_data$mtcars_ <- mtcars_
# r_data$mtcars_[5:20,2] <- NA
# getdata_exp("mtcars_", c("cyl","mpg"))
# getdata_exp("mtcars_", c("cyl","mpg"), filt = "mpg > 20")
# rm(mtcars_)
# getdata_exp("mtcars_", c("cyl","mpg"))
# getdata_exp("mtcars_", c("cyl","mpg"), filt = "mpg > 20")
# r_env <- new.env()
# session <- ""
# r_env$r_data <- r_data
# getdata_exp("mtcars_", c("cyl","mpg"))
# rm(r_data)
# getdata_exp("mtcars_", c("cyl","mpg"))
# rm(r_env)
# rm(session)
# getdata_exp("mtcars_", c("cyl","mpg"))
# getdata_exp("mycars", c("cyl","mpg"))
# getdata_exp("mtcars", c("cyl","mpg"), filt = "mpg > 20")
# end test

#' Make a .bat launcher for Windows on the desktop
#'
#' @param app App to install ('marketing', 'quant', or 'base'). Default is 'marketing'
#'
#' @return On a windows machine a file named 'radiant.bat' will be put on the desktop. Double-click the file to launch the Radiant app selected
#'
#' @export
win_launcher <- function(app = c("marketing", "quant", "base")) {

  # return("This function is for Windows only. For mac download launcher icons from http://mostly-harmless.github.io/radiant/index.html")
  if(.Platform$OS.type != 'windows')
    return("This function is for Windows only. For Mac use the mac_launcher() function")

  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

  filepath <- normalizePath(paste0(Sys.getenv("USERPROFILE") ,"/Desktop/"), winslash='/')
  launch_string <- paste0(Sys.which('R'), " -e \"if(!require(radiant)) { options(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/')); install.packages('radiant'); }; require(radiant); shiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\"")
  cat(launch_string,file=paste0(filepath,"radiant.bat"),sep="\n")
}


#' Make a .command launcher for Mac on the desktop
#'
#' @param app App to install ('marketing', 'quant', or 'base'). Default is 'marketing'
#'
#' @return On a mac a file named 'radiant.command' will be put on the desktop. Double-click the file to launch the Radiant app selected
#'
#' @export
mac_launcher <- function(app = c("marketing", "quant", "base")) {

  if(Sys.info()["sysname"] != "Darwin")
    return("This function is for Mac only. For windows use the win_launcher() function")

  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

  filename <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
  launch_string <- paste0("#!/usr/bin/env Rscript\n if(!require(radiant)) {\n options(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/'))\n install.packages('radiant')\n }\n\nrequire(radiant)\nshiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\n")
  cat(launch_string,file=filename,sep="\n")
  Sys.chmod(filename, mode = "0755")
}

# mac_launcher("base")
# mac_launcher("quant")
# mac_launcher()
# win_launcher()


