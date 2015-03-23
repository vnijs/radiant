descr_out <- function(descr, ret_type = 'html') {
   # if there is no data description
  if(descr %>% is_empty) return("")

  # if there is a data description and we want html output
  if(ret_type == 'html')
    descr <- markdown::markdownToHTML(text = descr, stylesheet="../base/www/empty.css")

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
  r_data[[paste0(objname,"_descr")]] <<- ret
  r_data[[objname]] <<- data.frame(matrix(rep("",12), nrow = 2))
}

loadClipboardData <- function(objname = "xls_data", ret = "", header = TRUE, sep = "\t") {

  if (.Platform$OS.type == 'windows') {
    dat <- try(read.table("clipboard", header = header, sep = sep), silent = TRUE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    dat <- try(read.table(pipe("pbpaste"), header = header, sep = sep), silent = TRUE)
  } else {
    dat <- try(read.table(text = input$load_cdata, header = header, sep = sep), silent = TRUE)
  }

  if(is(dat, 'try-error')) {
    if(ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
    upload_error_handler(objname,ret)
  } else {
    ret <- paste0("### Clipboard data\nData copied from clipboard on", lubridate::now())
    r_data[[objname]] <<- data.frame(dat, check.names = FALSE)
    # r_data[[objname]] <<- tbl_df(dat, check.names = FALSE)
    r_data[[paste0(objname,"description")]] <<- ret
  }
  r_data[['datasetlist']] <<- c(objname,r_data[['datasetlist']]) %>% unique
}

saveClipboardData <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == 'Windows') {
    write.table(.getdata(), "clipboard", sep="\t", row.names=FALSE)
  } else if (os_type == "Darwin") {
    write.table(.getdata(), file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
  } else if (os_type == "Linux") {
    print("### Saving data through the clipboard is currently only supported on Windows and Mac. You can save your data to csv format to use it in a spreadsheet.")
  }
}

loadUserData <- function(fname, uFile, ext, header = TRUE,
                         man_str_as_factor = TRUE, sep = ",",
                         dec = ".") {

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
      if(sum(robjname %in% c("r_state", "r_data")) == 2) {
        upload_error_handler(objname,"### To restore app state from a state-file please click the state radio button before uploading the file")
      } else {
        upload_error_handler(objname,"### More than one R object contained in the data.")
      }
    } else {
      r_data[[objname]] <<- as.data.frame(get(robjname))
      # r_data[[objname]] <<- tbl_df(get(robjname))
      r_data[[paste0(objname,"_descr")]] <<- attr(r_data[[objname]], "description")
    }
  }

  if(ext == 'csv') {
    # r_data[[objname]] <<- read.csv(uFile, header=header, sep=sep, dec=dec,
    r_data[[objname]] <<- read.table(uFile, header=header, sep=sep, dec=dec,
                                     stringsAsFactors=man_str_as_factor) # %>% tbl_df
  }

  r_data[['datasetlist']] <<- c(objname,r_data[['datasetlist']]) %>% unique
}
