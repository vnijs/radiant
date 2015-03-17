#' Launch Radiant in the default browser
#'
#' @details See \url{http://mostly-harmless.github.io/radiant} for documentation and tutorials
#'
#' @param app Choose the app to run. Either "base", "quant", or "marketing". "marketing" is the default
#'
#' @examples
#' if (interactive()) {
#'   radiant()
#' }
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

#' Alias used to set the class for analysis function return
#'
#' @examples
#' foo <- function(x) x^2 %>% set_class(c("foo", class(.)))
#'
#' @export
set_class <- `class<-`

#' Add stars '***' to a data.frame (from broom's `tidy` function) based on p.values
#'
#' @details Add stars to output from broom's `tidy` function
#'
#' @param pval Vector of p-values
#'
#' @return A vector of stars
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

#' Hide warnings and messages and return invisible
#'
#' @details Adapted from \url{http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/}
#'
#' @param ... Inputs to keep quite
#'
#' @examples
#' sshh( library(dplyr) )
#'
#' @export
sshh <- function(...) {
  suppressWarnings( suppressMessages( ... ) )
  invisible()
}

#' Hide warnings and messages and return result
#'
#' @details Adapted from \url{http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/}
#'
#' @param ... Inputs to keep quite
#'
#' @examples
#' sshhr( library(dplyr) )
#'
#' @export
sshhr <- function(...) {
  suppressWarnings( suppressMessages( ... ) )
}

#' Get data for analysis functions
#'
#' @param dataset Name of the dataframe
#' @param vars Variables to extract from the dataframe
#' @param na.rm Remove rows with missing values (default is TRUE)
#' @param filt Filter to apply to the specified dataset. For example "price > 10000" if dataset is "diamonds" (default is "")
#' @param slice Select a slice of the specified dataset. For example "1:10" for the first 10 rows or "n()-10:n()" for the last 10 rows (default is ""). Not in Radiant GUI
#'
#' @return Data.frame with specified columns and rows
#'
#' @examples
#' r_data <- list()
#' r_data$dat <- mtcars
#' getdata("dat","mpg:vs", filt = "mpg > 20", slice = "1:5")
#'
#' @export
getdata <- function(dataset,
                    vars = "",
                    na.rm = TRUE,
                    filt = "",
                    slice = "") {

  filt %<>% gsub("\\s","", .)

  { if(exists("r_env")) {
      r_env$r_data[[dataset]]
    } else if(exists("r_data") && !is.null(r_data[[dataset]])) {
      if(exists("running_local")) { if(running_local) cat("Dataset", dataset, "loaded from r_data list\n") }
      r_data[[dataset]]
    } else if(exists(dataset)) {
      d_env <- pryr::where(dataset)
      # cat("Dataset", dataset, "loaded from", environmentName(d_env), "environment\n")
      d_env[[dataset]]
    } else {
      paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
        stop %>% return
    }
  } %>% { if(filt == "") . else filter_(., filt) } %>%
        { if(slice == "") . else slice_(., slice) } %>%
        { if(vars[1] == "") . else select_(., .dots = vars) } %>%
        { if(na.rm) { if(anyNA(.)) na.omit(.) else . } }
}

#' Change data
#'
#' @param dataset Name of the dataframe to change
#' @param vars New variables to add to the data.frame
#' @param var_names Names for the new variables to add to the data.frame
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' r_data <- list()
#' r_data$dat <- data.frame(a = 1:20)
#' changedata("dat",20:1, "b")
#' head(r_data$dat)
#' }
#'
#' @export
changedata <- function(dataset,
                       vars = c(),
                       var_names = names(vars)) {

  if(exists("r_env")) {
    cat("Dataset", dataset, "changed in r_env\n")
    # r_env$r_data[[dataset]][,var_names] <<- vars
    r_env$r_data[[dataset]][,var_names] <- vars
  } else if(exists("r_data") && !is.null(r_data[[dataset]])) {
    if(exists("running_local")) { if(running_local) cat("Dataset", dataset, "changed in r_data list\n") }
    d_env <- pryr::where("r_data")
    d_env$r_data[[dataset]][,var_names] <- vars
  } else if(exists(dataset)) {
    d_env <- pryr::where(dataset)
    cat("Dataset", dataset, "changed in", environmentName(d_env), "environment\n")
    d_env[[dataset]][,var_names] <- vars
  } else {
    paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
      stop %>% return
  }
}

# adding variable when filter is active?
# dat <- mtcars
# dat$new <- NA
# dat$new[1:10] <- 1:10

#' Create a launcher for Windows (.bat)
#'
#' @details On Windows a file named `radiant.bat` will be put on the desktop. Double-click the file to launch the specified Radiant app
#'
#' @param app App to run when the desktop icon is double-clicked ("marketing", "quant", or "base"). Default is "marketing"
#'
#' @examples
#' if (interactive()) {
#'   if(Sys.info()["sysname"] != "Windows") {
#'     win_launcher()
#'     fn <- paste0(Sys.getenv("USERPROFILE") ,"/Desktop/radiant.bat")
#'     if(!file.exists(fn))
#'        stop("Windows launcher not created")
#'     else
#'       unlink(fn)
#'   }
#' }
#'
#' @export
win_launcher <- function(app = c("marketing", "quant", "base")) {

  if(Sys.info()["sysname"] != "Windows")
    return("This function is for Windows only. For Mac use the mac_launcher() function")

  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

  filepath <- normalizePath(paste0(Sys.getenv("USERPROFILE") ,"/Desktop/"), winslash='/')
  launch_string <- paste0(Sys.which('R'), " -e \"if(!require(radiant)) { options(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/')); install.packages('radiant'); }; require(radiant); shiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\"")
  cat(launch_string,file=paste0(filepath,"radiant.bat"),sep="\n")
}

#' Create a launcher for Mac (.command)
#'
#' @details On Mac a file named `radiant.command` will be put on the desktop. Double-click the file to launch the specified Radiant app
#'
#' @param app App to run when the desktop icon is double-clicked ("marketing", "quant", or "base"). Default is "marketing"
#'
#' @examples
#' if (interactive()) {
#'   if(Sys.info()["sysname"] != "Darwin") {
#'     mac_launcher()
#'     fn <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
#'     if(!file.exists(fn))
#'        stop("Mac launcher not created")
#'     else
#'       unlink(fn)
#'   }
#' }
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
