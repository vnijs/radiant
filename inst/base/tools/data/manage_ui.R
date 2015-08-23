#######################################
# Manage datasets in/out of Radiant
#######################################

output$ui_fileUpload <- renderUI({

  if (is.null(input$dataType)) return()
  if (input$dataType == "csv") {
    fileInput('uploadfile', '', multiple=TRUE,
              accept = c('text/csv','text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain','.csv','.tsv'))
  } else if (input$dataType == "rda") {
    fileInput('uploadfile', '', multiple=TRUE,
              accept = c(".rda",".rds",".rdata"))
  } else if (input$dataType == "url_rda") {
    with(tags, table(
      tr(
        td(textInput("url_rda", NULL, "")),
        td(actionButton("url_rda_load", "Load"), style="padding-top:5px;")
      )
    ))
  } else if (input$dataType == "url_csv") {
    with(tags, table(
      tr(
        td(textInput("url_csv", NULL, "")),
        td(actionButton("url_csv_load", "Load"), style="padding-top:5px;")
      )
    ))
  }
})

output$ui_clipboard_load <- renderUI({
  if (r_local) {
    actionButton('loadClipData', 'Paste data')
  } else {
    tagList(
      tags$textarea(class="form-control", id="load_cdata", rows="5"),
      actionButton('loadClipData', 'Paste data')
    )
  }
})

output$ui_clipboard_save <- renderUI({
  if (r_local) {
    actionButton('saveClipData', 'Copy data')
  } else {
    tagList(
      "<label>Add data description:</label><br>" %>% HTML,
      tags$textarea(class="form-control", id="save_cdata",
        rows="5",
        capture.output(write.table(.getdata(), file = "", row.names = FALSE,
                       sep = "\t")) %>%
          paste(collapse = "\n"))
    )
  }
})

output$ui_Manage <- renderUI({
  tagList(
    wellPanel(
      # radioButtons(inputId = "dataType", label = "Load data:",
      selectInput("dataType", label = "Load data of type:",
                   c("rda" = "rda", "rda (url)" = "url_rda", "csv" = "csv", "csv (url)" = "url_csv",
                     "clipboard" = "clipboard","examples" = "examples", "state" = "state"),
                     selected = "rda"),
      conditionalPanel(condition = "input.dataType != 'clipboard' &&
                                    input.dataType != 'examples'",
        conditionalPanel("input.dataType == 'csv' | input.dataType == 'url_csv'",
          with(tags, table(td(checkboxInput('man_header', 'Header', TRUE)),
            td(HTML("&nbsp;&nbsp;")),
            td(checkboxInput('man_str_as_factor', 'Str. as Factor', TRUE)))),
          radioButtons('man_sep', "Separator:", c(Comma=',', Semicolon=';', Tab='\t'),
                       ',', inline = TRUE),
          radioButtons('man_dec', "Decimal:", c(Period='.', Comma=','),
                       '.', inline = TRUE)
        ),
        uiOutput("ui_fileUpload")
      ),
      conditionalPanel(condition = "input.dataType == 'clipboard'",
        uiOutput("ui_clipboard_load")
      ),
      conditionalPanel(condition = "input.dataType == 'examples'",
        actionButton('loadExampleData', 'Load examples')
      ),
      conditionalPanel(condition = "input.dataType == 'state'",
        fileInput('uploadState', 'Load previous app state:',  accept = ".rda"),
        uiOutput("refreshOnUpload")
      )
    ),
    wellPanel(
      selectInput("saveAs", label = "Save data:",
      # radioButtons(inputId = "saveAs", label = "Save data:",
                   c("rda" = "rda", "csv" = "csv", "clipboard" = "clipboard",
                     "state" = "state"), selected = "rda"),
                     # "state" = "state"), selected = "rda", inline = TRUE),

      conditionalPanel(condition = "input.saveAs == 'clipboard'",
        uiOutput("ui_clipboard_save")
      ),
      conditionalPanel(condition = "input.saveAs != 'clipboard' &&
                                    input.saveAs != 'state'",
        downloadButton('downloadData', 'Save')
      ),
      conditionalPanel(condition = "input.saveAs == 'state'",
        HTML("<label>Save current app state:</label><br/>"),
        downloadButton('saveState', 'Save')
      )
    ),
    wellPanel(
      checkboxInput('man_show_remove', 'Remove data from memory', FALSE),
      conditionalPanel(condition = "input.man_show_remove == true",
        uiOutput("uiRemoveDataset"),
        actionButton('removeDataButton', 'Remove data')
      )
    ),
    help_modal('Manage','manage_help',inclMD(file.path(r_path,"base/tools/help/manage.md")))
  )
})

## updating the dataset description
observeEvent(input$updateDescr, {
  # if (is.null(input$updateDescr) || input$updateDescr == 0) return()
  isolate({
    r_data[[paste0(input$dataset,"_descr")]] <- input$man_data_descr
    updateCheckboxInput(session = session, "man_add_descr",
                        "Add/edit data description", FALSE)
  })
})

output$dataDescriptionHTML <- renderUI({
  r_data[[paste0(input$dataset,"_descr")]] %>%
    descr_out('html') %>%
    HTML
})

output$dataDescriptionMD <- renderUI({
  tagList(
    "<label>Add data description:</label><br>" %>% HTML,
    tags$textarea(class="form-control", id="man_data_descr",
                  rows="15", style="width:650px;",
                  descr_out(r_data[[paste0(input$dataset,"_descr")]], 'md'))
  )
})

# removing datasets
output$uiRemoveDataset <- renderUI({
  selectInput(inputId = "removeDataset", label = NULL,
    choices = r_data$datasetlist, selected = NULL, multiple = TRUE,
    size = length(r_data$datasetlist), selectize = FALSE
  )
})

observeEvent(input$removeDataButton, {
  # removing datasets
  # if (is.null(input$removeDataButton) || input$removeDataButton == 0) return()
  isolate({

    # only remove datasets if 1 or more were selected - without this line
    # all files would be removed when the removeDataButton is pressed
    if (is.null(input$removeDataset)) return()
    datasets <- r_data[['datasetlist']]
    if (length(datasets) > 1) {  # have to leave at least one dataset
      removeDataset <- input$removeDataset
      if (length(datasets) == length(removeDataset))
        removeDataset <- removeDataset[-1]

      # Must use single string to index into reactivevalues so loop is necessary
      for (rem in removeDataset) {
        r_data[[rem]] <- NULL
        r_data[[paste0(rem,"_descr")]] <- NULL
      }
      r_data[['datasetlist']] <- datasets[-which(datasets %in% removeDataset)]
    }
  })
})

# 'saving' data to clipboard
observeEvent(input$saveClipData, {
  # if (is.null(input$saveClipData) || input$saveClipData == 0) return()
  isolate({
    saveClipboardData()
    updateRadioButtons(session = session, inputId = "saveAs", selected = "rda")
  })
})

output$downloadData <- downloadHandler(
  filename = function() { paste0(input$dataset,'.',input$saveAs) },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$dataset

    if (ext == 'rda') {
      if (!is.null(input$man_data_descr) && input$man_data_descr != "") {
        # save data description
        dat <- .getdata()
        attr(dat,"description") <- r_data[[paste0(robj,"_descr")]]
        assign(robj, dat)
        save(list = robj, file = file)
      } else {
        assign(robj, .getdata())
        save(list = robj, file = file)
      }
    } else if (ext == 'csv') {
      write.csv(.getdata(), file, row.names = FALSE)
      # write_csv(.getdata(), file)
    }
  }
)

# loading data
# observe({
observeEvent(input$uploadfile, {
  # loading files from disk
  isolate({
    inFile <- input$uploadfile
    if (is.null(inFile)) return()
      # iterating through the files to upload
      for (i in 1:(dim(inFile)[1]))
        loadUserData(inFile[i,'name'], inFile[i,'datapath'], input$dataType,
                     header = input$man_header,
                     man_str_as_factor = input$man_str_as_factor,
                     sep = input$man_sep, dec = input$man_dec)

      updateSelectInput(session, "dataset", label = "Datasets:",
                        choices = r_data$datasetlist,
                        selected = r_data$datasetlist[1])
  })
})

observeEvent(input$url_rda_load, {
  ## loading rda file from url
  ## https://github.com/vnijs/radiant/blob/master/inst/examples/houseprices.rda?raw=true
  isolate({
    if (input$url_rda == "") return()
    objname <- "rda_url"
    con <- curl::curl(input$url_rda)
    try(open(con), silent = TRUE)
    if (is(con, 'try-error')) {
      upload_error_handler(objname, "### There was an error loading the r-data file from the provided url.")
    } else {
      robjname <- load(con)
      if (length(robjname) > 1) {
        if (sum(robjname %in% c("r_state", "r_data")) == 2) {
          upload_error_handler(objname,"### To restore app state from a state-file please choose the 'state' option from the dropdown.")
        } else {
          upload_error_handler(objname,"### More than one R object is contained in the specified data file.")
        }
      } else {
        r_data[[objname]] <- as.data.frame(get(robjname))
        r_data[[paste0(objname,"_descr")]] <- attr(r_data[[objname]], "description")
        r_data[['datasetlist']] <- c(objname, r_data[['datasetlist']]) %>% unique
        updateSelectInput(session, "dataset", label = "Datasets:",
                          choices = r_data$datasetlist,
                          selected = r_data$datasetlist[1])
      }
    }
    close(con)
  })
})

observeEvent(input$url_csv_load, {
  ## loading csv file from url
  ## https://raw.githubusercontent.com/vnijs/radiant/master/inst/examples/houseprices.csv
  isolate({
    objname <- "csv_url"
    if (input$url_csv == "") return()
    con <- curl::curl(input$url_csv)
    try(open(con), silent = TRUE)
    if (is(con, 'try-error')) {
      upload_error_handler(objname, "### There was an error loading the csv file from the provided url.")
    } else {

      dat <- try(read.table(con, header = input$man_header, comment.char = "",
                 quote = "\"", fill = TRUE, stringsAsFactors = input$man_str_as_factor,
                 sep = input$man_sep, dec = input$man_dec), silent = TRUE)

      if (is(dat, 'try-error'))
        upload_error_handler(objname, "### There was an error loading the csv file from the provided url.")
      else
        dat <- {if (input$man_str_as_factor) factorizer(dat) else dat} %>% as.data.frame

      r_data[[objname]] <- dat
      r_data[['datasetlist']] <- c(objname, r_data[['datasetlist']]) %>% unique

      updateSelectInput(session, "dataset", label = "Datasets:",
                        choices = r_data$datasetlist,
                        selected = r_data$datasetlist[1])
    }
    close(con)
  })
})

# loading all examples files (linked to help files)
observeEvent(input$loadExampleData, {
  # if (not_pressed(input$loadExampleData)) return()
  isolate({

    # loading data bundled with Radiant
    data_path <- file.path(r_path,"base/data/")
    examples <- list.files(data_path)

    for (ex in examples) loadUserData(ex, file.path(data_path,ex), 'rda')

    # loading data available for Rady students
    data_path <- "data/"
    examples <- list.files(data_path)

    for (ex in examples) loadUserData(ex, file.path(data_path,ex), 'rda')

    # sorting files alphabetically
    r_data[['datasetlist']] <- sort(r_data[['datasetlist']])

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist,
                      selected = r_data$datasetlist[1])
  })
})

observeEvent(input$loadClipData, {
  ## reading data from clipboard
  isolate({
    loadClipboardData()
    # updateRadioButtons(session = session, inputId = "dataType", selected = "rda")
    updateSelectInput(session = session, inputId = "dataType", selected = "rda")
    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist, selected = "copy_and_paste")
  })
})

#######################################
# Load previous state
#######################################
observe({
  inFile <- input$uploadState
  if (!is.null(inFile)) {
    isolate({
      tmpEnv <- new.env()
      load(inFile$datapath, envir=tmpEnv)

      r_sessions[[r_ssuid]] <- list(
        r_data = tmpEnv$r_data,
        r_state = tmpEnv$r_state,
        timestamp = Sys.time()
      )
      rm(tmpEnv)
    })
  }
})

output$refreshOnUpload <- renderUI({
  inFile <- input$uploadState
  if (!is.null(inFile)) {
    # Joe Cheng: https://groups.google.com/forum/#!topic/shiny-discuss/Olr8m0JwMTo
    tags$script("window.location.reload();")
  }
})

#######################################
# Save state
#######################################
saveState <- function(filename) {
  isolate({
    LiveInputs <- reactiveValuesToList(input)
    r_state[names(LiveInputs)] <- LiveInputs
    r_data <- reactiveValuesToList(r_data)
    save(r_state, r_data , file = filename)
  })
}

output$saveState <- downloadHandler(
  filename = function() { paste0("radiant-state-",Sys.Date(),".rda") },
  content = function(file) {
    saveState(file)
  }
)

#######################################
# Loading data into memory
#######################################
observeEvent(input$renameButton, {
  if (is_empty(input$data_rename)) return()
  isolate({
    ## use pryr::object_size to see that the size of the list doesn't change
    ## when you assign a list element another name
    r_data[[input$data_rename]] <- r_data[[input$dataset]]
    r_data[[input$dataset]] <- NULL
    r_data[[paste0(input$data_rename,"_descr")]] <- r_data[[paste0(input$dataset,"_descr")]]
    r_data[[paste0(input$dataset,"_descr")]] <- NULL

    ind <- which(input$dataset == r_data[['datasetlist']])
    r_data[['datasetlist']][ind] <- input$data_rename

    updateSelectInput(session, "dataset", label = "Datasets:", choices = r_data$datasetlist,
                      selected = input$data_rename)
  })
})

output$ui_datasets <- renderUI({
  ## Drop-down selection of active dataset
  tagList(
    selectInput(inputId = "dataset", label = "Datasets:", choices = r_data$datasetlist,
      selected = state_init("dataset"), multiple = FALSE),
    conditionalPanel(condition = "input.tabs_data == 'Manage'",
      checkboxInput("man_add_descr","Add/edit data description", FALSE),
      conditionalPanel(condition = "input.man_add_descr == true",
        actionButton('updateDescr', 'Update description')
      ),
      checkboxInput("man_rename_data","Rename data", FALSE),
      conditionalPanel(condition = "input.man_rename_data == true",
        uiOutput("uiRename")
      )
    )
  )
})

output$uiRename <- renderUI({
  tags$table(
    tags$td(textInput("data_rename", NULL, input$dataset)),
    tags$td(actionButton('renameButton', 'Rename'), style="padding-top:5px;")
  )
})

output$htmlDataExample <- renderText({

  if (is.null(.getdata())) return()

  ## Show only the first 10 (or 20) rows
  r_data[[paste0(input$dataset,"_descr")]] %>%
    { is_empty(.) %>% ifelse(., 20, 10) } %>%
    show_data_snippet(nshow = .)
})
