#' Launch Radiant in the default browser
#'
#' @details See \url{http://vnijs.github.io/radiant} for documentation and tutorials
#'
#' @param app Choose the app to run. One of "base", "quant", "analytics", "marketing". "analytics" is the default
#'
#' @examples
#' if (interactive()) {
#'   radiant("base")
#'   radiant("quant")
#'   radiant("marketing")
#'   radiant("analytics")
#' }
#' @export
radiant <- function(app = c("analytics", "marketing", "quant", "base")) {
  if (!"package:radiant" %in% search())
    if (!require(radiant)) stop("Calling radiant start function but radiant is not installed.")
  runApp(system.file(app[1], package = "radiant"), launch.browser = TRUE)
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
sshhr <- function(...) suppressWarnings( suppressMessages( ... ) )


#' Filter data with user-specified expression
#'
#' @param dat Data.frame to filter
#' @param filt Filter expression to apply to the specified dataset (e.g., "price > 10000" if dataset is "diamonds")
#'
#' @return Filtered data.frame
#'
#' @export
filterdata <- function(dat, filt = "") {
  if (grepl("([^=!<>])=([^=])", filt)) {
    message("Invalid filter: never use = in a filter but == (e.g., year == 2014). Update or remove the expression")
  } else {
    seldat <- try(filter_(dat, filt), silent = TRUE)
    if (is(seldat, 'try-error')) {
      message(paste0("Invalid filter: \"", attr(seldat,"condition")$message,"\". Update or remove the expression"))
    } else {
      return(droplevels(seldat))
    }
  }
  dat
}

#' Get data for analysis functions
#'
#' @param dataset Name of the dataframe
#' @param vars Variables to extract from the dataframe
#' @param filt Filter to apply to the specified dataset. For example "price > 10000" if dataset is "diamonds" (default is "")
#' @param rows Select rows in the specified dataset. For example "1:10" for the first 10 rows or "n()-10:n()" for the last 10 rows (default is NULL)
#' @param na.rm Remove rows with missing values (default is TRUE)
#'
#' @return Data.frame with specified columns and rows
#'
#' @examples
#' \donttest{
#' r_data <<- list()
#' r_data$dat <<- mtcars
#' getdata("dat","mpg:vs", filt = "mpg > 20", rows = 1:5)
#' rm(r_data, envir = .GlobalEnv)
#' }
#' @export
getdata <- function(dataset,
                    vars = "",
                    filt = "",
                    rows = NULL,
                    na.rm = TRUE) {

  # filt %<>% gsub("\\s","", .) %>% gsub("\"","\'",.)
  filt %<>% gsub("\\n","", .) %>% gsub("\"","\'",.)
  { if (!is_string(dataset)) {
      dataset
    } else if (exists("r_env") && !is.null(r_env$r_data[[dataset]])) {
      r_env$r_data[[dataset]]
    } else if (exists("r_data") && !is.null(r_data[[dataset]])) {
      if (exists("r_local")) { if (r_local) message("Dataset ", dataset, " loaded from r_data list\n") }
      r_data[[dataset]]
    } else if (exists(dataset)) {
      d_env <- pryr::where(dataset)
      # message("Dataset ", dataset, " loaded from ", environmentName(d_env), " environment\n")
      d_env[[dataset]]
    } else {
      message("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
        stop %>% return
    }
  } %>% { if ("grouped_df" %in% class(.)) ungroup(.) else . } %>%     # ungroup data if needed
        # { if (filt == "") . else filter_(., filt) } %>%     # apply data_filter
        { if (filt == "") . else filterdata(., filt) } %>%     # apply data_filter
        { if (is.null(rows)) . else slice(., rows) } %>%
        { if (vars[1] == "" || is.null(vars)) . else select_(., .dots = vars) } %>%
        { if (na.rm) na.omit(.) else . }
        ## line below may cause an error https://github.com/hadley/dplyr/issues/219
        # { if (na.rm) { if (anyNA(.)) na.omit(.) else . } else . }

  # use the below when all data is setup as tbl_df
  # } %>% { if (is.na(groups(.))) . else ungroup(.) } %>%     # ungroup data if needed
}

#' Convert character to factors as needed
#'
#' @param dat Data.frame
#' @param safx Values to levels ratio
#'
#' @return Data.frame with factors
#'
#' @export
factorizer <- function(dat, safx = 20) {
  isChar <- sapply(dat, is.character)
  if (sum(isChar) == 0) return(dat)
    toFct <-
      select(dat, which(isChar)) %>%
      summarise_each(funs(n_distinct(.) < 100 & (n_distinct(.)/length(.)) < (1/safx))) %>%
      select(which(. == TRUE)) %>% names
  if (length(toFct) == 0) return(dat)

  ## not using due to https://github.com/hadley/dplyr/issues/1238
  ## Seems fixed in dev version of dplyr
  # rmiss <- . %>% ifelse (is.na(.), "[Empty]", .) %>% ifelse (. == "", "[Empty]", .)
  # mutate_each_(dat, funs(rmiss), vars = toFct)  %>%  # replace missing levels
  mutate_each_(dat, funs(as.factor), vars = toFct)
}

#' Load an rda or rds file and add it to the radiant data list (r_data) if available
#'
#' @param fn File name and path as a string. Extension must be either rda or rds
#' @param objname Name to use for the data.frame. Defaults to the file name
#'
#' @return Data.frame in r_data or in the calling enviroment
#'
#' @export
loadr <- function(fn, objname = "") {

  filename <- basename(fn)
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("rda","rds")) {
    message("File must have extension rda or rds")
    return()
  }

  ## objname is used as the name of the data.frame
  if (objname == "")
    objname <- sub(paste0(".",ext,"$"),"", filename)

  if (ext == "rds") {
    loadfun <- readRDS
  } else {
    loadfun <- function(fn) load(fn) %>% get
  }

  if (exists("r_env") || exists("r_data")) {
    if (exists("r_env")) {
      env <- r_env
    } else if (exists("r_data")) {
      env <- pryr::where("r_data")
    }

    env$r_data[[objname]] <- loadfun(fn)
    env$r_data[[paste0(objname,"_descr")]] <- attr(env$r_data[[objname]], "description")
    env$r_data[['datasetlist']] <- c(objname, env$r_data[['datasetlist']]) %>% unique

  } else {
    assign(objname, loadfun(fn), envir = parent.frame())
  }
}

#' Save data.frame as an rda or rds file from Radiant
#'
#' @param objname Name of the data.frame
#' @param file File name and path as a string. Extension must be either rda or rds
#'
#' @return Data.frame in r_data
#'
#' @export
saver <- function(objname, file) {

  filename <- basename(file)
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("rda","rds")) {
    message("File must have extension rda or rds")
    return()
  }

  dat <- getdata(objname)

  if (ext == "rds") {
    saveRDS(dat, file = file)
  } else {
    assign(objname, dat)
    save(list = objname, file = file)
  }
}

#' Load a csv file with read.csv and read_csv
#'
#' @param fn File name string
#' @param header Header in file (TRUE, FALSE)
#' @param sep Use , (default) or ; or \\t
#' @param dec Decimal symbol. Use . (default) or ,
#' @param saf Convert character variables to factors if (1) there are less than 100 distinct values (2) there are X (see safx) more values than levels
#' @param safx Values to levels ratio
#'
#' @return Data.frame with (some) variables converted to factors
#'
#' @export
loadcsv <- function(fn, header = TRUE, sep = ",", dec = ".", saf = TRUE, safx = 20) {

  cn <- try(read.table(fn, header = header, sep = sep, comment.char = "", quote = "\"", fill = TRUE, stringsAsFactors = FALSE, nrows = 1), silent = TRUE)
  # dat <- try(read_delim(fn, sep, col_names = colnames(cn), skip = header), silent = TRUE) %>%
  try(read_delim(fn, sep, col_names = colnames(cn), skip = header), silent = TRUE) %>%
    {if (is(., 'try-error') || nrow(readr::problems(.)) > 0)
       try(read.table(fn, header = header, sep = sep, comment.char = "", quote = "\"", fill = TRUE, stringsAsFactors = FALSE), silent = TRUE)
     else . } %>%
    {if (is(., 'try-error'))
       return("### There was an error loading the data. Please make sure the data are in either rda or csv format.")
     else .} %>%
    {if (saf) factorizer(., safx) else . } %>% as.data.frame

  ## workaround for https://github.com/rstudio/DT/issues/161
  # isDate <- sapply(dat, is.Date)
  # if (sum(isDate) == 0) return(dat)
  # for (i in colnames(dat)[isDate]) dat[[i]] %<>% as.POSIXct %>% as.Date

  # dat
}

#' Load a csv file with from a url
#'
#' @param csv_url URL for the csv file
#' @param header Header in file (TRUE, FALSE)
#' @param sep Use , (default) or ; or \\t
#' @param dec Decimal symbol. Use . (default) or ,
#' @param saf Convert character variables to factors if (1) there are less than 100 distinct values (2) there are X (see safx) more values than levels
#' @param safx Values to levels ratio
#'
#' @return Data.frame with (some) variables converted to factors
#'
#' @importFrom curl curl
#'
#' @export
loadcsv_url <- function(csv_url, header = TRUE, sep = ",", dec = ".", saf = TRUE, safx = 20) {

  con <- curl(csv_url)
  try(open(con), silent = TRUE)
  if (is(con, 'try-error')) {
    close(con)
    return("### There was an error loading the csv file from the provided url.")
  } else {
    dat <- try(read.table(con, header = header, comment.char = "",
               quote = "\"", fill = TRUE, stringsAsFactors = saf,
               sep = sep, dec = dec), silent = TRUE)
    close(con)

    if (is(dat, 'try-error'))
      return("### There was an error loading the data. Please make sure the data are in csv format.")

    if (saf) dat <- factorizer(dat, safx)

    dat
  }
}

#' Load an rda file from a url
#'
#' @param rda_url URL for the csv file
#'
#' @return Data.frame
#'
#' @importFrom curl curl
#'
#' @export
loadrda_url <- function(rda_url) {
  con <- curl(rda_url)
  try(open(con), silent = TRUE)
  if (is(con, 'try-error')) {
    close(con)
    return("### There was an error loading the rda file from the provided url.")
  } else {
    robj <- load(con)
    if (length(robj) > 1) message("The connection contains multiple R-objects. Only the first will be returned.")
    close(con)
    get(robj)
  }
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
    message("Dataset ", dataset, " changed in r_env\n")
    r_env$r_data[[dataset]][,var_names] <- vars
  } else if (exists("r_data") && !is.null(r_data[[dataset]])) {
    if (exists("r_local")) { if (r_local) message("Dataset ", dataset, " changed in r_data list\n") }
    d_env <- pryr::where("r_data")
    d_env$r_data[[dataset]][,var_names] <- vars
  } else if (exists(dataset)) {
    d_env <- pryr::where(dataset)
    message("Dataset ", dataset, " changed in ", environmentName(d_env), " environment\n")
    d_env[[dataset]][,var_names] <- vars
  } else {
    paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
      stop %>% return
  }
}

## use ensurer to check if nrow(vars) == nrow(dataset)
## http://cran.r-project.org/web/packages/ensurer/vignettes/ensurer.html

## adding variable when filter is active?
# dat <- mtcars
# dat$new <- NA
# dat$new[1:10] <- 1:10

#' View data
#'
#' @details View, search, sort, etc. your data
#'
#' @param dataset Name of the dataframe to change
#' @param vars Variables to show (default is all)
#' @param filt Filter to apply to the specified dataset. For example "price > 10000" if dataset is "diamonds" (default is "")
#' @param rows Select rows in the specified dataset. For example "1:10" for the first 10 rows or "n()-10:n()" for the last 10 rows (default is NULL)
#' @param na.rm Remove rows with missing values (default is FALSE)
#'
#' @examples
#' if (interactive()) {
#' viewdata(mtcars)
#' viewdata("mtcars")
#' mtcars %>% viewdata
#' }
#'
#' @export
viewdata <- function(dataset,
                     vars = "",
                     filt = "",
                     rows = NULL,
                     na.rm = FALSE) {

  ## based on http://rstudio.github.io/DT/server.html
  dat <- getdata(dataset, vars, filt = filt, rows = rows, na.rm = FALSE)
  title <- if (is_string(dataset)) paste0("DT:", dataset) else "DT"

  if (nrow(dat) > 5000000) {
    fbox <- "none"
  } else {
    fbox <- list(position = "top")
    dc <- getclass(dat)
    if ("factor" %in% dc) {
      toChar <- sapply(select(dat, which(dc == "factor")), function(x) length(levels(x))) > 100
      if (any(toChar))
        dat <- mutate_each_(dat, funs(as.character), vars = names(toChar)[toChar])
    }
  }

  shinyApp(
    ui = fluidPage(title = title,
      includeCSS(file.path(system.file(package = "radiant"),"base/www/style.css")),
      fluidRow(DT::dataTableOutput("tbl")),
      tags$button(id = "stop", type = "button",
                  class = "btn btn-danger action-button shiny-bound-input",
                  onclick = "window.close();", "Stop")
    ),
    server = function(input, output, session) {
      widget <- DT::datatable(dat, selection = "none",
        rownames = FALSE, style = "bootstrap",
        filter = fbox, escape = FALSE,
        # extensions = 'KeyTable'# ,
        options = list(
          search = list(regex = TRUE),
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE,
          processing = FALSE,
          pageLength = 10,
          lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
        )
      )
      output$tbl <- DT::renderDataTable(widget)
      observeEvent(input$stop, {stopApp("Stopped viewdata")})
    }
  )
}

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
    sub("Date","date", .) %>%
    sub("Period","period", .)
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
is_empty <- function(x, empty = "") if (length(x) == 0 || is.na(x) || x == empty) TRUE else FALSE

#' Is input a string?
#'
#' @details Is input a string
#'
#' @param x Input
#'
#' @return TRUE if string, else FALSE
#'
#' @examples
#' is_string("")
#' is_string("data")
#' is_string(c("data","data"))
#' is_string(NULL)
#'
#' @export
is_string <- function(x)
  if (is.character(x) && length(x) == 1 && !is_empty(x)) TRUE else FALSE

#' Create a vector of interaction terms
#'
#' @param vars Variables lables to use
#' @param nway 2-way (2) or 3-way (3) interactions labels to create
#' @param sep Separator between variable names (default is :)
#'
#' @return Character vector of interaction term labels
#'
#' @examples
#' paste0("var", 1:3) %>% iterms(2)
#' paste0("var", 1:3) %>% iterms(3)
#' paste0("var", 1:3) %>% iterms(2, sep = ".")
#'
#' @export
iterms <- function(vars, nway, sep = ":") {
  if (!nway %in% c(2,3)) return(character(0))
  it <- c()
  for (i in 2:nway) {
    it %<>% {c(., combn(vars, i) %>% apply(2, paste, collapse = sep))}
    ## lm doesn't evaluate a:a
    # if (i == 2) it <- c(it, paste(vars, vars, sep = "*"))
    # if (i == 3) it <- c(it, paste(vars, vars, vars, sep = "*"))
  }
  it
}

#' Create a launcher and updater for Windows (.bat)
#'
#' @details On Windows a file named 'radiant.bat' and one named 'update_radiant.bat' will be put on the desktop. Double-click the file to launch the specified Radiant app or update Radiant to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
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
win_launcher <- function(app = c("analytics", "marketing", "quant", "base")) {

  if (!interactive()) stop("This function can only be used in an interactive R session")

  if (Sys.info()["sysname"] != "Windows")
    return(message("This function is for Windows only. For Mac use the mac_launcher() function"))

  answ <- readline("Do you want to create shortcuts for Radiant on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y","Y")) {

    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

    # pt <- normalizePath(paste0(Sys.getenv("USERPROFILE") ,"/Desktop/"), winslash='/')
    pt <- file.path(Sys.getenv("HOME") ,"Desktop")
    if (!file.exists(pt))
      pt <- file.path(Sys.getenv("USERPROFILE") ,"Desktop", fsep = "\\")

    if (!file.exists(pt)) {
      pt <- Sys.getenv("HOME")
      message(paste0("The launcher function was unable to find your Desktop. The launcher and update files/icons will be put in the directory: ", pt))
    }

    pt <- normalizePath(pt, winslash='/')

    fn1 <- file.path(pt, "radiant.bat")
    launch_string <- paste0("\"",Sys.which('R'), "\" -e \"if (!require(radiant)) { install.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/', type = 'binary') }; library(radiant); shiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\"")
    cat(launch_string, file=fn1, sep="\n")
    Sys.chmod(fn1, mode = "0755")

    fn2 <- file.path(pt, "update_radiant.bat")
    launch_string <- paste0("\"", Sys.which('R'), "\" -e \"unlink('~/r_sessions/*.rds', force = TRUE); install.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/', type = 'binary')\"\npause(1000)")
    cat(launch_string,file=fn2,sep="\n")
    Sys.chmod(fn2, mode = "0755")

    if (file.exists(fn1) && file.exists(fn2))
      message("Done! Look for a file named radiant.bat on your desktop. Double-click it to start Radiant in your default browser. There is also a file called update_radiant.bat you can double click to update the version of Radiant on your computer.\n")
    else
      message("Something went wrong. No shortcuts were created.")
  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher and updater for Mac (.command)
#'
#' @details On Mac a file named 'radiant.command' and one named 'update_radiant.command' will be put on the desktop. Double-click the file to launch the specified Radiant app or update Radiant to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
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
mac_launcher <- function(app = c("analytics", "marketing", "quant", "base")) {

  if (!interactive()) stop("This function can only be used in an interactive R session")

  if (Sys.info()["sysname"] != "Darwin")
    return(message("This function is for Mac only. For windows use the win_launcher() function"))

  answ <- readline("Do you want to create shortcuts for Radiant on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y","Y")) {

    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

    fn1 <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
    launch_string <- paste0("#!/usr/bin/env Rscript\nif (!require(radiant)) {\n  install.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/', type = 'binary')\n}\n\nlibrary(radiant)\nshiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\n")
    cat(launch_string,file=fn1,sep="\n")
    Sys.chmod(fn1, mode = "0755")

    fn2 <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/update_radiant.command")
    launch_string <- paste0("#!/usr/bin/env Rscript\nunlink('~/r_sessions/*.rds', force = TRUE)\ninstall.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/', type = 'binary')\nSys.sleep(1000)")
    cat(launch_string,file=fn2,sep="\n")
    Sys.chmod(fn2, mode = "0755")

    if (file.exists(fn1) && file.exists(fn2))
      message("Done! Look for a file named radiant.command  on your desktop. Double-click it to start Radiant in your default browser. There is also a file called update_radiant.command you can double click to update the version of Radiant on your computer.\n")
    else
      message("Something went wrong. No shortcuts were created.")

  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher and updater for Linux (.sh)
#'
#' @details On Linux a file named 'radiant.sh' and one named 'update_radiant.sh' will be put on the desktop. Double-click the file to launch the specified Radiant app or update Radiant to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
#'
#' @examples
#' if (interactive()) {
#'   if (Sys.info()["sysname"] == "Linux") {
#'     lin_launcher()
#'     fn <- paste0("/home/",Sys.getenv("USER"),"/Desktop/radiant.sh")
#'     if (!file.exists(fn))
#'       stop("Linux launcher not created")
#'     else
#'       unlink(fn)
#'   }
#' }
#'
#' @export
lin_launcher <- function(app = c("analytics", "marketing", "quant", "base")) {

  if (!interactive()) stop("This function can only be used in an interactive R session")

  if (Sys.info()["sysname"] != "Linux")
    return(message("This function is for Linux only. For windows use the win_launcher() function and for mac use the mac_launcher() function"))

  answ <- readline("Do you want to create shortcuts for Radiant on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y","Y")) {

    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

    fn1 <- paste0("/home/",Sys.getenv("USER"),"/Desktop/radiant.sh")
    launch_string <- paste0("#!/usr/bin/env Rscript\nif (!require(radiant)) {\n  install.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/')\n}\n\nlibrary(radiant)\nshiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\n")
    cat(launch_string,file=fn1,sep="\n")
    Sys.chmod(fn1, mode = "0755")

    fn2 <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/update_radiant.sh")
    launch_string <- paste0("#!/usr/bin/env Rscript\nunlink('~/r_sessions/*.rds', force = TRUE)\ninstall.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/')\nsleep(1000)")
    cat(launch_string,file=fn2,sep="\n")
    Sys.chmod(fn2, mode = "0755")

    if (file.exists(fn1) && file.exists(fn2))
      message("Done! Look for a file named radiant.sh on your desktop. Double-click it to start Radiant in your default browser. There is also a file called update_radiant.sh you can double click to update the version of Radiant on your computer.\n")
    else
      message("Something went wrong. No shortcuts were created.")

  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher on the desktop for Windows (.bat), Mac (.command), or Linux (.sh)
#'
#' @details On Windows/Mac/Linux a file named radiant.bat/radiant.command/radiant.sh will be put on the desktop. Double-click the file to launch the specified Radiant app
#'
#' @seealso \code{\link{win_launcher}} to create a shortcut on Windows
#' @seealso \code{\link{mac_launcher}} to create a shortcut on Mac
#' @seealso \code{\link{lin_launcher}} to create a shortcut on Linux
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
#'
#' @export
launcher <- function(app = c("analytics", "marketing", "quant", "base")) {

  os <- Sys.info()["sysname"]

  if (os == "Darwin")
    mac_launcher(app[1])
  else if (os == "Windows")
    win_launcher(app[1])
  else if (os == "Linux")
    lin_launcher(app[1])
  else
    return(message("This function is not available for your platform."))
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

#' Import all functions that a package imports for use with Shiny
#'
#' @param .from The package to pull the function from
#'
#' @examples
#' \donttest{
#' copy_imported(radiant)
#' }
#' @export
copy_imported <- function(.from) {

  ## not yet working as intended
  # from <- as.character(substitute(.from))

  # import_list <- getNamespaceImports(from)
  # parent  <- parent.frame()
  # import_names <- names(import_list)

  # for (i in unique(import_names)) {
  #   if (i %in% c("base","shiny","magrittr")) next

  #   symbols <- unlist(import_list[which(i == import_names)])

  #   for (j in symbols) {
  #     # do.call(import::from, list(i = as.symbol(i), j = as.symbol(j)))
  #     fn <- get(j, envir = asNamespace(i), inherits = TRUE)
  #     assign(j, eval.parent(call("function", formals(fn), body(fn))), parent)
  #   }
  # }

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
  # from    <- as.character(substitute(.from))

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
  if (!exists("r_state")) stop("Make sure to use copy_from inside shinyServer for the state_* functions")
  # if (is.null(r_state[[inputvar]])) init else r_state[[inputvar]]
  if (is_empty(r_state[[inputvar]])) init else r_state[[inputvar]]
}

# state_init <- function(inputvar, init = "", pf = parent.frame()) {
# print(parent.frame())
# r_state %>% { if (is.null(.[[inputvar]])) init else .[[inputvar]] }

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
  if (!exists("r_state")) stop("Make sure to use copy_from inside shinyServer for the state_* functions")
  # r_state %>% { if (is.null(.[[inputvar]])) init else vals[vals == .[[inputvar]]] }
  r_state %>% { if (is_empty(.[[inputvar]])) init else vals[vals == .[[inputvar]]] }
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
  if (!exists("r_state")) stop("Make sure to use copy_from inside shinyServer for the state_* functions")
  r_state %>%
    # { if (is.null(.[[inputvar]]))
    { if (is_empty(.[[inputvar]]))
        ## "a" %in% character(0) --> FALSE, letters[FALSE] --> character(0)
        vals[vals %in% init]
      else
        vals[vals %in% .[[inputvar]]]
    }
}

#' Print/draw method for grobs produced by gridExtra
#'
#' @details Print method for ggplot grobs created using arrangeGrob. Code is based on \url{https://github.com/baptiste/gridextra/blob/master/inst/testing/shiny.R}
#'
#' @param x a gtable object
#' @param ... further arguments passed to or from other methods
#'
#' @return A plot
#'
#' @export
print.gtable <- function(x, ...) {
  if (is.ggplot(x)) x <- ggplotGrob(x)
  grid::grid.draw(x)
}

#' Labels for confidence intervals
#'
#' @param alt Type of hypothesis ("two.sided","less","greater")
#' @param cl Confidence level
#'
#' @return A character vector with labels for a confidence interval
#'
#' @examples
#' ci_label("less",.95)
#' ci_label("two.sided",.95)
#' ci_label("greater",.9)
#'
#' @export
ci_label <- function(alt = "two.sided", cl = .95) {
  if (alt == "less") {
    c("0%", paste0(100*cl,"%"))
  } else if (alt == "greater") {
    c(paste0(100*(1-cl),"%"), "100%")
  } else {
    {100 * (1-cl)/2} %>%
      c(., 100 - .) %>%
      round(1) %>%
      paste0(.,"%")
  }
}

#' Values at confidence levels
#'
#' @param dat Data
#' @param alt Type of hypothesis ("two.sided","less","greater")
#' @param cl Confidence level
#'
#' @return A vector with values at a confidence level
#'
#' @examples
#' ci_perc(0:100, "less",.95)
#' ci_perc(0:100, "greater",.95)
#' ci_perc(0:100, "two.sided",.80)
#'
#' @export
ci_perc <- function(dat, alt = "two.sided", cl = .95) {
  probs <- if (alt == 'two.sided') {
    ((1-cl)/2) %>% c(., 1 - .)
  } else if (alt == 'less') {
    1-cl
  } else {
    cl
  }
  quantile(dat, probs = probs)
}

#' Print a data.frame with a specified number of decimal places
#'
#' @param tbl Data.frame
#' @param dec Number of decimal places
#' @param perc Display numbers as percentages (TRUE or FALSE)
#'
#' @return Data.frame for printing
#'
#' @examples
#' data.frame(x = c("a","b"), y = c(1L, 2L), z = c(-0.0005, 3)) %>%
#'   dfprint(dec = 3)
#' @export
dfprint <- function(tbl, dec = 3, perc = FALSE) {
  if (perc) {
    tbl %<>% mutate_each(
      funs(if (is.numeric(.)) . * 100L else .)
    )
  }

  frm <- if (perc) "f%%" else "f"
  tbl %>%
  mutate_each(
    funs(if (is.double(.)) sprintf(paste0("%.", dec ,frm), .) else .)
  )

}

#' Print a number with a specified number of decimal places, thousand sep, and a symbol
#'
#' @param x Number or vector
#' @param dec Number of decimal places
#' @param sym Symbol to use
#' @param perc Display number as a percentage
#'
#' @return Character (vector) in the desired format
#'
#' @examples
#' nrprint(2000, "$")
#' nrprint(2000, dec = 4)
#' nrprint(.05, perc = TRUE)
#' nrprint(c(.1, .99), perc = TRUE)
#' nrprint(data.frame(a = c(.1, .99)), perc = TRUE)
#' nrprint(data.frame(a = 1000), sym = "$", dec = 0)
#'
#' @export
nrprint <- function(x, sym = "", dec = 2, perc = FALSE) {
  if ("data.frame" %in% class(x)) x <- x[[1]]
  if (perc)
    paste0(sym, formatC(100 * x, digits = dec, big.mark = ",", format = "f"), "%")
  else
    paste0(sym, formatC(x, digits = dec, big.mark = ",", format = "f"))
}

#' Round double in a data.frame to a specified number of decimal places
#'
#' @param tbl Data.frame
#' @param dec Number of decimal places
#'
#' @return Data.frame for viewing
#'
#' @examples
#' data.frame(x = c("a","b"), y = c(1L, 2L), z = c(-0.0005, 3.1)) %>%
#'   dfround(dec = 3)
#' @export
dfround <- function(tbl, dec = 3) {
  tbl %>%
  mutate_each(
    funs(if (is.double(.)) round(., dec) else .)
  )
}
