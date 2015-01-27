descr_out <- function(descr, ret_type = 'html') {
   # if there is no data description
  if(descr %>% is_empty) return("")

  # if there is a data description and we want html output
  if(ret_type == 'html')
    descr <- markdownToHTML(text = descr, stylesheet="../base/www/empty.css")

  descr
}

#### test
# library(markdown)
# is_empty("## header example")
# is_empty(NULL)
# descr_out(NULL)
# descr_out("## header example", 'html')
# descr_out("## header example", 'md')
#### end test

upload_error_handler <- function(objname, ret) {
  # create an empty data.frame and return error message as description
  values[[paste0(objname,"_descr")]] <<- ret
  values[[objname]] <<- data.frame(matrix(rep("",12), nrow = 2))
}

loadClipboardData <- function(objname = "xls_data", ret = "") {

  if (.Platform$OS.type == 'windows') {
    dat <- try(read.table("clipboard", header = TRUE, sep = '\t'), silent = TRUE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    dat <- try(read.table(pipe("pbpaste"), header = TRUE, sep = '\t'), silent = TRUE)
  } else {
    ret <- c("### Loading data through the clipboard is not currently supported online.")
    dat <- try(log("a"), silent = TRUE)   # create try-error
  }

  if(is(dat, 'try-error')) {
    if(ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
    upload_error_handler(objname,ret)
  } else {
    ret <- paste0("### Clipboard data\nData copied from clipboard on", lubridate::now())
    values[[objname]] <<- data.frame(dat, check.names = FALSE)
    values[[paste0(objname,"description")]] <<- ret
  }
  values[['datasetlist']] <<- c(objname,values[['datasetlist']]) %>% unique
}

saveClipboardData <- function() {
  os_type <- .Platform$OS.type
  if (os_type == 'windows') {
    write.table(getdata(), "clipboard", sep="\t", row.names=FALSE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    write.table(getdata(), file = pipe("pbcopy"), row.names = FALSE,
                sep = '\t')
  } else {
    print("### Saving data through the clipboard is not currently supported online.")
  }
}

loadUserData <- function(fname, uFile, ext, header = TRUE,
                         man_str_as_factor = TRUE, sep = ",") {

  filename <- basename(fname)
  # objname is used as the name of the data.frame
  objname <- sub(paste0(".",ext,"$"),"", filename)

  # if ext isn't in the filename ...
  if(objname == filename) {
    fext <- tools::file_ext(filename) %>% tolower

    if(fext %in% c("xls","xlsx")) {
      ret <- "### Radiant does not load xls files directly. Please save the data as a csv file and try again."
    } else {
      ret <- paste0("### The filename extension (",fext,") does not match the specified file-type (",ext,"). Please specify the file type you are trying to upload (i.e., csv or rda).")
    }

    upload_error_handler(objname,ret)
    ext <- "---"
  }

  if(ext == 'rda') {
    # objname will hold the name of the object(s) inside the R datafile
    robjname <- try(load(uFile), silent=TRUE)
    if(is(robjname, 'try-error')) {
      upload_error_handler(objname, "### There was an error loading the data. Please make sure the data are in either rda or csv format.")
      return()
    } else if(length(robjname) > 1) {
      if(sum(robjname %in% c("state_list", "values")) == 2) {
        upload_error_handler(objname,"### To restore app state from a state-file please click the state radio button before uploading the file")
      } else {
        upload_error_handler(objname,"### More than one R object contained in the data.")
      }
    } else {
      values[[objname]] <<- as.data.frame(get(robjname))
      values[[paste0(objname,"_descr")]] <<- attr(values[[objname]], "description")
    }
  }

  if(ext == 'csv') {
    values[[objname]] <<- read.csv(uFile, header=header,
                                  sep=sep,
                                  stringsAsFactors=man_str_as_factor)
  }

  values[['datasetlist']] <<- c(objname,values[['datasetlist']]) %>% unique
}

# rm(list = ls())
# source("~/gh/radiant_dev/R/radiant.R")
# source("~/gh/radiant_dev/inst/base/radiant.R")
# library(dplyr)
# values <- list()
# fname <- normalizePath("~/Desktop/GitHub/radiant_dev/tests/test_data/houseprices.csv")
# filename <- fname
# loadUserData(fname, fname, "csv")
# values
# loadUserData(fname, fname, "rda")
# values
# fname <- normalizePath("~/Desktop/GitHub/radiant_dev/tests/test_data/houseprices.csv")
# loadUserData(fname, fname, "csv")
# values
# fname <- normalizePath("~/Desktop/GitHub/radiant_dev/tests/test_data/houseprices.xlsx")
# loadUserData(fname, fname, "rda")
# values
# fname <- normalizePath("~/Desktop/GitHub/radiant_dev/tests/test_data/RadiantState-2015-01-18.rda")
# loadUserData(fname, fname, "rda")
# values

# fname <- normalizePath("~/Desktop/GitHub/radiant_dev/example_data/RadiantState.rda")
# loadUserData(fname, fname, "rda")
# values[['datasetlist']][1]

# robjname <- try(load(fname), silent=TRUE)
# robjname
# length(robjname) > 1
# sum(robjname %in% c("state_list", "values")) == 2

