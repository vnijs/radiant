#######################################
# Manage datasets in/out of Radiant
#######################################

output$ui_fileUpload <- renderUI({

  if(is.null(input$dataType)) return()

  if(input$dataType == "csv") {
    fileInput('uploadfile', '', multiple=TRUE,
              accept = c('text/csv', 'text/comma-separated-values', 'text/tab-separated-values', 'text/plain', '.csv', '.tsv'))
  } else if(input$dataType == "rda") {
    fileInput('uploadfile', '', multiple=TRUE, accept = c(".rda",".rds"))
  }
})

output$ui_Manage <- renderUI({
  list(wellPanel(
      radioButtons(inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"),
        selected = "rda"),
      conditionalPanel(condition = "input.dataType != 'clipboard' && input.dataType != 'examples'",
        conditionalPanel(condition = "input.dataType == 'csv'",
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', '', c(Comma=',', Semicolon=';', Tab='\t'), ',')
        ),
        uiOutput("ui_fileUpload")
      ),
      conditionalPanel(condition = "input.dataType == 'clipboard'",
        actionButton('loadClipData', 'Paste data')
      ),
      conditionalPanel(condition = "input.dataType == 'examples'",
        actionButton('loadExampleData', 'Load examples')
      )
    ),
    wellPanel(
      radioButtons(inputId = "saveAs", label = "Save data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard"),
        selected = "rda"),
      checkboxInput("man_add_descr","Add/edit data description", FALSE),
      conditionalPanel(condition = "input.man_add_descr == true",
        actionButton('updateDescr', 'Update description')
      ),
      conditionalPanel(condition = "input.saveAs == 'clipboard'",
        actionButton('saveClipData', 'Copy data')
      ),
      conditionalPanel(condition = "input.saveAs != 'clipboard' && input.man_add_descr == false",
        downloadButton('downloadData', 'Save')
      )
    ),
    wellPanel(
      textInput("data_rename", "", input$datasets),
      actionButton('renameButton', 'Rename dataset')
     ),
    wellPanel(
      uiOutput("uiRemoveDataset"),
      actionButton('removeDataButton', 'Remove data')
    ),
    helpModal('Manage','manageHelp',inclMD("../base/tools/help/manage.md"))
  )
})

# updating the dataset description
observe({
  if(is.null(input$updateDescr) || input$updateDescr == 0) return()
  isolate({
    values[[paste0(input$datasets,"_descr")]] <- input$man_data_descr
    updateCheckboxInput(session = session, "man_add_descr","Add/edit data description", FALSE)
  })
})

observe({
  if(is.null(input$data_rename)) return()
  if(is.null(input$renameButton) || input$renameButton == 0) return()

  isolate({
    values[[input$data_rename]] <- getdata()
    values[[input$datasets]] <- NULL
    values[[paste0(input$data_rename,"_descr")]] <- values[[paste0(input$datasets,"_descr")]]
    values[[paste0(input$datasets,"_descr")]] <- NULL

    ind <- which(input$datasets == values[['datasetlist']])
    values[['datasetlist']][ind] <- input$data_rename

    updateSelectInput(session, "datasets", label = "Datasets:", choices = values$datasetlist,
      selected = input$data_rename)
  })
})

# managing the description of the dataset
dataDescriptionOutput <- function(ret = 'html') {
  descr <- values[[paste0(input$datasets,"_descr")]]
  if(is.null(descr) || descr == "") {
    return("")  # if there is no data description
  } else {
    # if there is a data description and the 'add/edit' box has been checked
    ifelse(ret == 'md',return(descr),
      return(suppressWarnings(markdownToHTML(text = descr, stylesheet="../base/www/empty.css"))))
  }
}

# removing datasets
output$uiRemoveDataset <- renderUI({
  # Drop-down selection of data set to remove
  selectInput(inputId = "removeDataset", label = "Remove data from memory:",
    choices = values$datasetlist, selected = NULL, multiple = TRUE, selectize = FALSE)
})

observe({
  # removing datasets
  if(is.null(input$removeDataButton) || input$removeDataButton == 0) return()
  isolate({

    # only remove datasets if 1 or more were selected
    # without this line all files would be removed when
    # the removeDataButton is pressed
    if(is.null(input$removeDataset)) return()

    datasets <- values[['datasetlist']]
    if(length(datasets) > 1) {         # don't remove the last dataset
      removeDataset <- input$removeDataset
      if(length(datasets) == length(removeDataset)) {
        # datasets <- ""
        removeDataset <- removeDataset[-1]
      }
      for(rem in removeDataset) {
        values[[rem]] <- NULL
      }
      datasets <- datasets[-which(datasets %in% removeDataset)]
      values[['datasetlist']] <- datasets
    }
  })
})

# saving data
observe({
  # 'saving' data to clipboard
  if(is.null(input$saveClipData) || input$saveClipData == 0) return()
  isolate({
    os_type <- .Platform$OS.type
    if (os_type == 'windows') {
      write.table(getdata(), "clipboard", sep="\t", row.names=FALSE)
    } else {
      write.table(getdata(), file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
    }
    updateRadioButtons(session = session, inputId = "saveAs", label = "Save data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard"), selected = ".rda")
  })
})

output$downloadData <- downloadHandler(
  filename = function() { paste(input$datasets,'.',input$saveAs, sep='') },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$datasets

    if(ext == 'rda') {
      if(input$man_data_descr != "") {

        # save data description
        dat <- getdata()
        attr(dat,"description") <- values[[paste0(robj,"_descr")]]
        assign(robj, dat)
        save(list = robj, file = file)
      } else {
        assign(robj, getdata())
        save(list = robj, file = file)
      }
    } else if(ext == 'csv') {
      assign(robj, getdata())
      write.csv(get(robj), file)
    }
  }
)

# loading data
observe({
  # loading files from disk
  inFile <- input$uploadfile
  if(!is.null(inFile) && !is.na(inFile)) {
    isolate({
      # iterating through the files to upload
      for(i in 1:(dim(inFile)[1])) loadUserData(inFile[i,'name'], inFile[i,'datapath'], input$dataType)
    })
  }
})

# loading all examples files (linked to helpfiles)
observe({
  if(is.null(input$loadExampleData) || input$loadExampleData == 0) return()
  isolate({

    # loading data bundled with Radiant
    path <- "data/data_examples/"
    examples <- list.files(path)

    for(ex in examples) loadUserData(ex, paste0(path,ex), 'rda')

    # loading data available for Rady students
    path <- "data/data_rady/"
    examples <- list.files(path)

    for(ex in examples) loadUserData(ex, paste0(path,ex), 'rda')

    # sorting files alphabetically
    values[['datasetlist']] <- sort(values[['datasetlist']])
  })
})

observe({
  # 'reading' data from clipboard
  if(is.null(input$loadClipData) || input$loadClipData == 0) return()
  isolate({
    os_type <- .Platform$OS.type
    if (os_type == 'windows') {

      dat <- try(read.table("clipboard", header = TRUE, sep = '\t'), silent = TRUE)
      if(is(dat, 'try-error')) dat <- c("Data from clipboard was not well formatted. Try exporting the data to csv format.")
    } else {

      dat <- try(read.table(pipe("pbpaste"), header = TRUE, sep = '\t'), silent = TRUE)
      if(is(dat, 'try-error')) dat <- c("Data from clipboard was not well formatted. Try exporting the data to csv format.")
    }

    values[['xls_data']] <- as.data.frame(dat)
    values[['datasetlist']] <- unique(c('xls_data',values[['datasetlist']]))
    updateRadioButtons(session = session, inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"), selected = ".rda")
  })
})

loadUserData <- function(filename, uFile, ext) {

  objname <- sub(paste(".",ext,sep = ""),"",basename(filename))

#   ext <- tolower(tools::file_ext(filename))
#   validate(
#     need(ext %in% c('rda','rds','rdata','csv'), message = "Based on the filename extension this does not seems to be a filetype that Radiant can load. Radiant currently supports R-data files with extension .rda, .rds, and .rdata.\n Files exported from Excel in .csv format can also be loaded. Excel files in .xls or .xlsx format must either (1) be  converted to csv format or (2) copy-and-pasted into Radiant using the clipboard option. See the helpfile for additional details.")
#   )

  if(ext == 'rda') {
    # objname will hold the name of the object(s) inside the R datafile
    robjname <- load(uFile)

    if(length(robjname) > 1) {
      # keeping this bit for legacy
      values[[objname]] <- as.data.frame(get(robjname[-which(robjname == "description")]))
      values[[paste0(objname,"_descr")]] <- get("description")
    } else {
      values[[objname]] <- as.data.frame(get(robjname))
      values[[paste0(objname,"_descr")]] <- attr(values[[objname]], "description")
    }
  }

  if(length(values[['datasetlist']]) == 0 || values[['datasetlist']][1] == '') {
    values[['datasetlist']] <- c(objname)
  } else {
    values[['datasetlist']] <- unique(c(objname,values[['datasetlist']]))
  }

  if(ext == 'csv') {
    values[[objname]] <- read.csv(uFile, header=input$header, sep=input$sep)
  }
}

output$uiDatasets <- renderUI({
  # Drop-down selection of data set
  selectInput(inputId = "datasets", label = "Datasets:", choices = values$datasetlist,
    selected = state_init("datasets"), multiple = FALSE)
})

output$htmlDataExample <- renderText({

  # if(isolate(input$datatabs) != 'Manage') return(invisible())
  dat <- getdata()
  if(is.null(dat)) return()

  # Show only the first 10 rows
  nr <- min(10,nrow(dat))
  dat <- data.frame(dat[1:nr,, drop = FALSE])
  dat <- date2character_dat(dat) # dealing with dates
  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- sub("<table border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html
})
