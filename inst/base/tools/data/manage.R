descr_out <- function(descr, ret_type = 'html') {
   ## if there is no data description
  if (descr %>% is_empty) return("")

  ## if there is a data description and we want html output
  if (ret_type == 'html')
    descr <- markdown::markdownToHTML(text = descr, stylesheet = "")

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
  ## create an empty data.frame and return error message as description
  r_data[[paste0(objname,"_descr")]] <- ret
  r_data[[objname]] <- data.frame(matrix(rep("",12), nrow = 2))
}

loadClipboardData <- function(objname = "xls_data", ret = "", header = TRUE, sep = "\t") {

  dat <- sshhr(try(
         {if (Sys.info()["sysname"] == "Windows") {
            read.table("clipboard", header = header, sep = sep, as.is = TRUE)
          } else if (Sys.info()["sysname"] == "Darwin") {
            read.table(pipe("pbpaste"), header = header, sep = sep, as.is = TRUE)
          } else {
            if (!is_empty(input$load_cdata))
              read.table(text = input$load_cdata, header = header, sep = sep, as.is = TRUE)
          }} %>% as.data.frame(check.names = FALSE), silent = TRUE))

  if (is(dat, 'try-error') || nrow(dat) == 0) {
    if (ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
    upload_error_handler(objname,ret)
  } else {
    ret <- paste0("### Clipboard data\nData copied from clipboard on", lubridate::now())
    r_data[[objname]] <- dat
    r_data[[paste0(objname,"_descr")]] <- ret
  }
  r_data[['datasetlist']] <- c(objname,r_data[['datasetlist']]) %>% unique
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

factorizer <- function(dat) {
  isChar <- sapply(dat,is.character)
  if (sum(isChar) == 0) return(dat)
    toFct <-
      select(dat, which(isChar)) %>%
      summarise_each(funs(n_distinct(.) < 100 & (n_distinct(.)/length(.)) < .1)) %>%
      select(which(. == TRUE)) %>% names
    # summarise_each(funs(n_distinct)) %>%
    # select(which(. < 100 & ((. / nrow(dat)) < .1))) %>% names
  if (length(toFct) == 0) return(dat)
  mutate_each_(dat, funs(as.factor), vars = toFct)
}

loadUserData <- function(fname, uFile, ext,
                         header = TRUE,
                         man_str_as_factor = TRUE,
                         sep = ",",
                         dec = ".") {

  filename <- basename(fname)
  ## objname is used as the name of the data.frame
  objname <- sub(paste0(".",ext,"$"),"", filename)

  ## if ext isn't in the filename ...
  if (objname == filename) {
    fext <- tools::file_ext(filename) %>% tolower

    if (fext %in% c("xls","xlsx")) {
      ret <- "### Radiant does not load xls files directly. Please save the data as a csv file and try again."
    } else {
      ret <- paste0("### The filename extension (",fext,") does not match the specified file-type (",ext,"). Please specify the file type you are trying to upload (i.e., csv or rda).")
    }

    upload_error_handler(objname,ret)
    ext <- "---"
  }

  if (ext == 'rda') {
    ## objname will hold the name of the object(s) inside the R datafile
    robjname <- try(load(uFile), silent = TRUE)
    if (is(robjname, 'try-error')) {
      upload_error_handler(objname, "### There was an error loading the data. Please make sure the data are in either rda or csv format.")
    } else if (length(robjname) > 1) {
      if (sum(robjname %in% c("r_state", "r_data")) == 2) {
        upload_error_handler(objname,"### To restore app state from a state-file please click the state radio button before uploading the file")
      } else {
        upload_error_handler(objname,"### More than one R object contained in the data.")
      }
    } else {
      r_data[[objname]] <- as.data.frame(get(robjname))
      r_data[[paste0(objname,"_descr")]] <- attr(r_data[[objname]], "description")
    }
  }

  if (ext == 'csv') {
    r_data[[objname]] <- try(read_delim(uFile, sep, col_names=header), silent = TRUE) %>%
      {if (is(., 'try-error'))
          try(read.table(uFile, header = header, sep = sep, dec = dec, stringsAsFactors = FALSE), silent = TRUE)
        else . } %>%
      {if (is(., 'try-error'))
          upload_error_handler(objname, "### There was an error loading the data. Please make sure the data are in either rda or csv format.")
        else . } %>%
      {if (man_str_as_factor) factorizer(.) else . } %>% as.data.frame

    # r_data[[objname]] <- try(read.table(uFile, header=header, sep=sep, dec=dec,
    #   stringsAsFactors=FALSE), silent = TRUE) %>%
    #   { if (is(., 'try-error')) upload_error_handler(objname, "### There was an error loading the data. Please make sure the data are in either rda or csv format.")
    #     else . } %>%
    #   { if (man_str_as_factor) factorizer(.) else . } # %>% tbl_df
  }

  r_data[['datasetlist']] <- c(objname, r_data[['datasetlist']]) %>% unique
}
