# managing the description of the dataset
dataDescriptionOutput <- function(ret = 'html') {
  descr <<- values[[paste0(input$dataset,"_descr")]]
  if(is.null(descr) || descr == "") {
    return("")  # if there is no data description
  } else {
    # if there is a data description and the 'add/edit' box has been checked
    ifelse(ret == 'md',return(descr),
      return(suppressWarnings(markdownToHTML(text = descr,
             stylesheet="../base/www/empty.css"))))
  }
}

upload_error_handler <- function(objname, ret) {
  values[[paste0(objname,"_descr")]] <<- ret
  values[[objname]] <<- data.frame(matrix(rep("",12), nrow = 2))
}

loadClipboardData <- function() {
  ret <- ""
  if (.Platform$OS.type == 'windows') {
    dat <- try(read.table("clipboard", header = TRUE, sep = '\t'), silent = TRUE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    dat <- try(read.table(pipe("pbpaste"), header = TRUE, sep = '\t'), silent = TRUE)
  } else {
    dat <- try(log("a"), silent = TRUE)
    ret <- c("### Loading data through the clipboard is not currently supported on Radiant server.")
  }

  if(is(dat, 'try-error')) {
    if(ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
    upload_error_handler("xls_data",ret)
  } else {
    values[['xls_data']] <<- data.frame(dat, check.names = FALSE)
    values[['xls_data_description']] <<- ret
    values[['datasetlist']] <<- unique(c('xls_data',values[['datasetlist']]))
  }
}

saveClipboardData <- function() {
  os_type <- .Platform$OS.type
  if (os_type == 'windows') {
    write.table(getdata(), "clipboard", sep="\t", row.names=FALSE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    write.table(getdata(), file = pipe("pbcopy"), row.names = FALSE,
                sep = '\t')
  } else {
    print("Saving data through the clipboard is not currently supported on Radiant server.")
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
      upload_error_handler(objname, "There was an error loading the data. Please make sure the data are in either rda or csv format.")
      return()
    } else if(length(robjname) > 1) {
      if(sum(robjname %in% c("state_list", "values")) == 2) {
        upload_error_handler("state_file","### To restore app state from a state-file please click the state radio button before uploading the file")
      } else {
        upload_error_handler("load error","###  ")
      }
    } else {
      values[[objname]] <<- as.data.frame(get(robjname))
      values[[paste0(objname,"_descr")]] <<- attr(values[[objname]], "description")
    }
  }

  # if(length(values[['datasetlist']]) == 0 || values[['datasetlist']][1] == '') {
  #   values[['datasetlist']] <- c(objname)
  # } else {
    # values[['datasetlist']] <- unique(c(objname,values[['datasetlist']]))
  # }

  # header <- TRUE
  # sep <- ","
  # man_str_as_factor <- TRUE
  # uFile <- filename


  if(ext == 'csv') {
    values[[objname]] <<- read.csv(uFile, header=header,
                                  sep=sep,
                                  stringsAsFactors=man_str_as_factor)
  }

  values[['datasetlist']] <<- unique(c(objname,values[['datasetlist']]))
}

# rm(list = ls())
# library(dplyr)
# values <- list()
# fname <- normalizePath("~/Desktop/Car Survey MDS 2015.csv")
# # fname <- normalizePath("~/Desktop/GitHub/radiant_dev/tests/test_data/houseprices.csv")
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
