#' Launch Radiant in the default browser
#'
#' @details See \url{http://vnijs.github.io/radiant} for documentation and tutorials
#'
#' @param app Choose the app to run. Either "base", "quant", or "marketing". "marketing" is the default
#'
#' @examples
#' if (interactive()) {
#'   radiant("base")
#'   radiant("quant")
#'   radiant("marketing")
#' }
#' @export
radiant <- function(app = c("marketing", "quant", "base"))
  runApp(system.file(app[1], package="radiant"), launch.browser = TRUE)

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
sshhr <- function(...)
  suppressWarnings( suppressMessages( ... ) )

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
#' \donttest{
#' r_data <<- list()
#' r_data$dat <<- mtcars
#' getdata("dat","mpg:vs", filt = "mpg > 20", slice = "1:5")
#' rm(r_data, envir = .GlobalEnv)
#' }
#' @export
getdata <- function(dataset,
                    vars = "",
                    na.rm = TRUE,
                    filt = "",
                    slice = "") {

  filt %<>% gsub("\\s","", .)

  { if (exists("r_env")) {
      r_env$r_data[[dataset]]
    } else if (exists("r_data") && !is.null(r_data[[dataset]])) {
      if (exists("r_local")) { if (r_local) cat("Dataset", dataset, "loaded from r_data list\n") }
      r_data[[dataset]]
    } else if (exists(dataset)) {
      d_env <- pryr::where(dataset)
      # cat("Dataset", dataset, "loaded from", environmentName(d_env), "environment\n")
      d_env[[dataset]]
    } else {
      paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
        stop %>% return
    }
  } %>% { if ("grouped_df" %in% class(.)) ungroup(.) else . } %>%     # ungroup data if needed
        { if (filt == "") . else filter_(., filt) } %>%     # apply data_filter
        { if (slice == "") . else slice_(., slice) } %>%
        { if (vars[1] == "") . else select_(., .dots = vars) } %>%
        { if (na.rm) { if (anyNA(.)) na.omit(.) else . } }

  # use the below when all data is setup as tbl_df
  # } %>% { if (is.na(groups(.))) . else ungroup(.) } %>%     # ungroup data if needed
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
#' \donttest{
#' r_data <<- list()
#' r_data$dat <<- data.frame(a = 1:20)
#' changedata("dat",20:1, "b")
#' head(r_data$dat)
#' rm(r_data, envir = .GlobalEnv)
#' }
#'
#' @export
changedata <- function(dataset,
                       vars = c(),
                       var_names = names(vars)) {

  if (exists("r_env")) {
    cat("Dataset", dataset, "changed in r_env\n")
    r_env$r_data[[dataset]][,var_names] <- vars
  } else if (exists("r_data") && !is.null(r_data[[dataset]])) {
    if (exists("r_local")) { if (r_local) cat("Dataset", dataset, "changed in r_data list\n") }
    d_env <- pryr::where("r_data")
    d_env$r_data[[dataset]][,var_names] <- vars
  } else if (exists(dataset)) {
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

#' Get variable class
#'
#' @details Get variable class information for each column in a data.frame
#'
#' @param dat Dataset to evaluate
#'
#' @return Vector with class information for each variable
#'
#' @examples
#' getclass(mtcars)
#'
#' @export
getclass <- function(dat) {
  sapply(dat, function(x) class(x)[1]) %>%
    sub("ordered","factor", .) %>%
    sub("POSIXct","date", .) %>%
    sub("POSIXlt","date", .) %>%
    sub("Date","date", .)
}

#' Is a character variable defined
#'
#' @details Is a variable NULL or an empty string
#'
#' @param x Character value to evaluate
#' @param empty Indicate what 'empty' means. Default is empty string (i.e., "")
#'
#' @return TRUE if empty, else FALSE
#'
#' @examples
#' is_empty("")
#' is_empty(NULL)
#'
#' @export
is_empty <- function(x, empty = "") if (is.null(x) || x == empty) TRUE else FALSE

#' Create a launcher for Windows (.bat)
#'
#' @details On Windows a file named 'radiant.bat' will be put on the desktop. Double-click the file to launch the specified Radiant app
#'
#' @param app App to run when the desktop icon is double-clicked ("marketing", "quant", or "base"). Default is "marketing"
#'
#' @examples
#' if (interactive()) {
#'   if (Sys.info()["sysname"] == "Windows") {
#'     win_launcher()
#'     fn <- paste0(Sys.getenv("USERPROFILE") ,"/Desktop/radiant.bat")
#'     if (!file.exists(fn))
#'       stop("Windows launcher not created")
#'     else
#'       unlink(fn)
#'   }
#' }
#'
#' @export
win_launcher <- function(app = c("marketing", "quant", "base")) {

  if (!interactive()) stop("This function can only be used in an interactive R session")

  if (Sys.info()["sysname"] != "Windows")
    return(cat("This function is for Windows only. For Mac use the mac_launcher() function"))

  answ <- readline("Do you want to create a shortcut for Radiant on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y","Y")) {

    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

    filename <- normalizePath(paste0(Sys.getenv("USERPROFILE") ,"/Desktop/"), winslash='/') %>%
                  paste0("radiant.bat")
    launch_string <- paste0(Sys.which('R'), " -e \"if (!require(radiant)) { options(repos = 'http://vnijs.github.io/radiant_miniCRAN/'); install.packages('radiant'); }; library(radiant); shiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\"")
    cat(launch_string, file=filename, sep="\n")

    if (file.exists(filename))
      cat("Done! Look for a file named radiant.bat on your desktop. Double-click it to start Radiant in your default browser.\n")
    else
      cat("Something went wrong. No shortcut was created.")

  } else {
    cat("No shortcut was created.\n")
  }
}

#' Create a launcher for Mac (.command)
#'
#' @details On Mac a file named 'radiant.command' will be put on the desktop. Double-click the file to launch the specified Radiant app
#'
#' @param app App to run when the desktop icon is double-clicked ("marketing", "quant", or "base"). Default is "marketing"
#'
#' @examples
#' if (interactive()) {
#'   if (Sys.info()["sysname"] == "Darwin") {
#'     mac_launcher()
#'     fn <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
#'     if (!file.exists(fn))
#'       stop("Mac launcher not created")
#'     else
#'       unlink(fn)
#'   }
#' }
#'
#' @export
mac_launcher <- function(app = c("marketing", "quant", "base")) {

  if (!interactive()) stop("This function can only be used in an interactive R session")

  if (Sys.info()["sysname"] != "Darwin")
    return(cat("This function is for Mac only. For windows use the win_launcher() function"))

  answ <- readline("Do you want to create a shortcut for Radiant on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y","Y")) {

    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

    filename <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
    launch_string <- paste0("#!/usr/bin/env Rscript\nif (!require(radiant)) {\n  options(repos = 'http://vnijs.github.io/radiant_miniCRAN/')\n  install.packages('radiant')\n}\n\nlibrary(radiant)\nshiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\n")
    cat(launch_string,file=filename,sep="\n")
    Sys.chmod(filename, mode = "0755")

    if (file.exists(filename))
      cat("Done! Look for a file named radiant.command on your desktop. Double-click it to start Radiant in your default browser.\n")
    else
      cat("Something went wrong. No shortcut was created.")

  } else {
    cat("No shortcut was created.\n")
  }
}

#' Create a launcher for Mac (.command)
#'
#' @details On Mac (Windows) a file named radiant.command (radiant.bat) will be put on the desktop. Double-click the file to launch the specified Radiant app
#'
#' @seealso \code{\link{mac_launcher}} to create a shortcut on mac
#' @seealso \code{\link{mac_launcher}} to create a shortcut on windows
#'
#' @param app App to run when the desktop icon is double-clicked ("marketing", "quant", or "base"). Default is "marketing"
#'
#' @export
launcher <- function(app = c("marketing", "quant", "base")) {

  if (Sys.info()["sysname"] == "Darwin")
    mac_launcher(app[1])
  else if (Sys.info()["sysname"] == "Windows")
    win_launcher(app[1])
  else
    return(cat("This function is for Mac and Windows only."))
}

#' Source for package functions
#'
#' @details Equivalent of source with local=TRUE for package functions. Written by smbache, author of the import package. See \url{https://github.com/smbache/import/issues/4} for a discussion. This function will be depracated when (if) it is included in \url{https://github.com/smbache/import}
#'
#' @param .from The package to pull the function from
#' @param ... Functions to pull
#'
#' @examples
#'
#' copy_from(radiant, state_init)
#'
#' @export
copy_from <- function(.from, ...) {

  # copied from import:::symbol_list and import:::symbol_as_character by @smbache
  dots <- eval(substitute(alist(...)), parent.frame(), parent.frame())
  names <- names(dots)
  unnamed <- if (is.null(names)) 1:length(dots)
             else which(names == "")
  dots <- vapply(dots, as.character, character(1))
  names(dots)[unnamed] <- dots[unnamed]

  symbols <- dots
  parent  <- parent.frame()
  from    <- as.character(substitute(.from))

  for (s in seq_along(symbols)) {
    fn <- get(symbols[s], envir = asNamespace(from), inherits = TRUE)
    assign(names(symbols)[s],
           eval.parent(call("function", formals(fn), body(fn))),
           parent)
  }

  invisible(NULL)
}

#' Source all package functions
#'
#' @details Equivalent of source with local=TRUE for all package functions. Adapted from functions by smbache, author of the import package. See \url{https://github.com/smbache/import/issues/4} for a discussion. This function will be depracated when (if) it is included in \url{https://github.com/smbache/import}
#'
#' @param .from The package to pull the function from
#'
#' @examples
#'
#' copy_all(radiant)
#'
#' @export
copy_all <- function(.from) {

  from <- as.character(substitute(.from))
  ls(getNamespace(from), all.names=TRUE) %>%
    .[grep("^\\.", ., invert = TRUE)] %>%
    set_names(.,.) -> symbols

  parent  <- parent.frame()
  from    <- as.character(substitute(.from))

  for (s in seq_along(symbols)) {
    fn <- get(symbols[s], envir = asNamespace(from), inherits = TRUE)
    assign(names(symbols)[s],
           eval.parent(call("function", formals(fn), body(fn))),
           parent)
  }

  invisible(NULL)
}

#' Set initial value for shiny input
#'
#' @details Useful for radio button or checkbox
#'
#' @param inputvar Name shiny input
#' @param init Initial value to use if state value for input not set
#'
#' @return value for inputvar
#'
#' @examples
#'
#' r_state <<- list()
#' state_init("test")
#' state_init("test",0)
#' r_state$test <- c("a","b")
#' state_init("test",0)
#' shiny::radioButtons("rb", label = "Button:", c("a","b"), selected = state_init("rb", "a"))
#' r_state$rb <- "b"
#' shiny::radioButtons("rb", label = "Button:", c("a","b"), selected = state_init("rb", "a"))
#' rm(r_state)
#'
#' @seealso \code{\link{state_single}}
#' @seealso \code{\link{state_multiple}}
#' @seealso \code{\link{copy_from}}
#'
#' @export
state_init <- function(inputvar, init = "") {
  if (!exists("r_state")) stop(cat("Make sure to use copy_from inside shinyServer for the state_* functions"))
  r_state %>% { if (is.null(.[[inputvar]])) init else .[[inputvar]] }
}

#' Set initial value for shiny input from a list of values
#'
#' @details Useful for select input with multiple = FALSE
#'
#' @param inputvar Name shiny input
#' @param vals Possible values for inputvar
#' @param init Initial value to use if state value for input not set
#'
#' @return value for inputvar
#'
#' @examples
#'
#' r_state <- list()
#' state_single("test",1:10,1)
#' r_state$test <- 8
#' state_single("test",1:10,1)
#' shiny::selectInput("si", label = "Select:", c("a","b"), selected = state_single("si"))
#' r_state$si <- "b"
#' shiny::selectInput("si", label = "Select:", c("a","b"), selected = state_single("si", "b"))
#'
#' @seealso \code{\link{state_init}}
#' @seealso \code{\link{state_multiple}}
#' @seealso \code{\link{copy_from}}
#'
#' @export
state_single <- function(inputvar, vals, init = character(0)) {
  if (!exists("r_state")) stop(cat("Make sure to use copy_from inside shinyServer for the state_* functions"))
  r_state %>% { if (is.null(.[[inputvar]])) init else vals[vals == .[[inputvar]]] }
}

#' Set initial values for shiny input from a list of values
#'
#' @details Useful for select input with multiple = TRUE and when you want to use inputs selected for another tool (e.g., pre_factor and full_factor or hier_clus and kmeans_clus in Radiant)
#'
#' @param inputvar Name shiny input
#' @param vals Possible values for inputvar
#' @param init Initial value to use if state value for input not set
#'
#' @return value for inputvar
#'
#' @examples
#'
#' r_state <- list()
#' state_multiple("test",1:10,1:3)
#' r_state$test <- 8:10
#' state_multiple("test",1:10,1:3)
#' shiny::selectInput("sim", label = "Select:", c("a","b"),
#'   selected = state_multiple("sim", c("a","b")),  multiple = TRUE)
#' r_state$sim <- c("a","b")
#' shiny::selectInput("sim", label = "Select:", c("a","b"),
#'   selected = state_single("sim", c("a","b")),  multiple = TRUE)
#'
#' @seealso \code{\link{state_init}}
#' @seealso \code{\link{state_single}}
#' @seealso \code{\link{copy_from}}
#'
#' @export
state_multiple <- function(inputvar, vals, init = character(0)) {
  if (!exists("r_state")) stop(cat("Make sure to use copy_from inside shinyServer for the state_* functions"))
  r_state %>%
    { if (is.null(.[[inputvar]]))
        # "a" %in% character(0) --> FALSE, letters[FALSE] --> character(0)
        vals[vals %in% init]
      else
        vals[vals %in% .[[inputvar]]]
    }
}
