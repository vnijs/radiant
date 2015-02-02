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
getdata_exp <- function(dataset, vars, na.rm = TRUE, filt = NULL) {

  # filt <- ifelse(filt %>% is.null, f(...), filter())
  clean <- ifelse(na.rm, na.omit, f(...))
  if(exists("r_env")) {
    # cat("Dataset", dataset, "loaded from the radiant environment (r_env)\n")
    select_(r_env$r_data[[dataset]], .dots = vars) %>% clean
  } else if(exists("r_data") && !is.null(r_data[[dataset]])) {
    if(running_local) cat("Dataset", dataset, "loaded from r_data list\n")
    select_(r_data[[dataset]], .dots = vars) %>% clean
  } else if(exists(dataset)) {
    cat("Dataset", dataset, "loaded from global environment\n")
    select_(get(dataset), .dots = vars) %>% clean
  } else {
    paste0("Dataset ", dataset, " is not available. Please load the dataset and put the name in the function call") %>%
      stop %>% return
  }
}

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


# test
# rm(list = ls())
# library(dplyr)
# mtcars_ <- mtcars
# getdata_exp("mtcars_", c("cyl","mpg"))
# r_data <- list()
# r_data$mtcars_ <- mtcars_
# r_data$mtcars_[5:20,2] <- NA
# getdata_exp("mtcars_", c("cyl","mpg"))
# rm(mtcars_)
# getdata_exp("mtcars_", c("cyl","mpg"))
# r_env <- new.env()
# r_env$r_data <- r_data
# getdata_exp("mtcars_", c("cyl","mpg"))
# rm(r_data)
# getdata_exp("mtcars_", c("cyl","mpg"))
# rm(r_env)
# getdata_exp("mtcars_", c("cyl","mpg"))
# getdata_exp("mycars", c("cyl","mpg"))
# end test
