#######################################
# Manage datasets in/out of Radiant
#######################################

output$ui_fileUpload <- renderUI({

  if(input$dataType %>% is.null) return()
  if(input$dataType == "csv") {
    fileInput('uploadfile', '', multiple=TRUE,
              accept = c('text/csv','text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain','.csv','.tsv'))
  } else if(input$dataType == "rda") {
    fileInput('uploadfile', '', multiple=TRUE,
              accept = c(".rda",".rds",".rdata"))
  }
})

output$ui_clipboard_load <- renderUI({
  if(running_local) {
    actionButton('loadClipData', 'Paste data')
  } else {
    tagList(tags$textarea(class="form-control",
      id="load_cdata", rows="5"
    ),
    actionButton('loadClipData', 'Paste data'))
  }
})

output$ui_clipboard_save <- renderUI({
  if(running_local) {
    actionButton('saveClipData', 'Copy data')
  } else {
    tagList(
      "<label>Add data description:</label><br>" %>% HTML,
      tags$textarea(class="form-control", id="save_cdata",
        rows="5",
        capture.output(write.table(.getdata(), file = "", row.names = FALSE, sep = "\t")) %>%
          paste(collapse = "\n"))
    )
  }
})

output$ui_Manage <- renderUI({
  list(
    wellPanel(
      # shinyFilesButton('file', 'File select', 'Please select a file', FALSE),
      radioButtons(inputId = "dataType", label = "Load data:",
                   c("rda" = "rda", "csv" = "csv",  "clipboard" = "clipboard",
                     "examples" = "examples", "state" = "state"),
                     selected = "rda", inline = TRUE),
      conditionalPanel(condition = "input.dataType != 'clipboard' &&
                                    input.dataType != 'examples'",
        conditionalPanel(condition = "input.dataType == 'csv'",
          checkboxInput('header', 'Header', TRUE),
          checkboxInput('man_str_as_factor', 'String as Factor', TRUE),
          radioButtons('sep', NULL, c(Comma=',', Semicolon=';', Tab='\t'), ',',
                       inline = TRUE)
        ),
        uiOutput("ui_fileUpload")
      ),
      conditionalPanel(condition = "input.dataType == 'clipboard'",
        # actionButton('loadClipData', 'Paste data')
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
      radioButtons(inputId = "saveAs", label = "Save data:",
                   c("rda" = "rda", "csv" = "csv", "clipboard" = "clipboard",
                     "state" = "state"), selected = "rda", inline = TRUE),

      conditionalPanel(condition = "input.saveAs == 'clipboard'",
        # actionButton('saveClipData', 'Copy data')
        uiOutput("ui_clipboard_save")
      ),
      conditionalPanel(condition = "input.saveAs != 'clipboard' &&
                                    input.saveAs != 'state'",
        downloadButton('downloadData', 'Save')
      ),
      conditionalPanel(condition = "input.saveAs == 'state'",
        HTML("<label>Save current app state:</label><br/>"),
        downloadButton('downloadState', 'Save')
      )
    ),
    wellPanel(
      checkboxInput('man_show_remove', 'Remove data from memory', FALSE),
      conditionalPanel(condition = "input.man_show_remove == true",
        uiOutput("uiRemoveDataset"),
        actionButton('removeDataButton', 'Remove data')
      )
    ),
    help_modal('Manage','manageHelp',inclMD("../base/tools/help/manage.md"))
  )
})

# updating the dataset description
observe({
  if(is.null(input$updateDescr) || input$updateDescr == 0) return()
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

observe({
  # removing datasets
  if(is.null(input$removeDataButton) || input$removeDataButton == 0) return()
  isolate({

    # only remove datasets if 1 or more were selected - without this line
    # all files would be removed when the removeDataButton is pressed
    if(is.null(input$removeDataset)) return()
    datasets <- r_data[['datasetlist']]
    if(length(datasets) > 1) {  # have to leave at least one dataset
      removeDataset <- input$removeDataset
      if(length(datasets) == length(removeDataset))
        removeDataset <- removeDataset[-1]

      # Must use single string to index into reactivevalues so loop is necessary
      for(rem in removeDataset) {
        r_data[[rem]] <- NULL
        r_data[[paste0(rem,"_descr")]] <- NULL
      }
      r_data[['datasetlist']] <- datasets[-which(datasets %in% removeDataset)]
    }
  })
})

# 'saving' data to clipboard
observe({
  if(is.null(input$saveClipData) || input$saveClipData == 0) return()
  isolate({
    saveClipboardData()
    updateRadioButtons(session = session, inputId = "saveAs",
                       label = "Save data:",
                       c("rda" = "rda", "csv" = "csv", "clipboard" = "clipboard",
                         "state" = "state"), selected = "rda", inline = TRUE)
  })
})

output$downloadData <- downloadHandler(
  filename = function() { paste0(input$dataset,'.',input$saveAs) },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$dataset

    if(ext == 'rda') {
      if(!is.null(input$man_data_descr) && input$man_data_descr != "") {
        # save data description
        dat <- .getdata()
        attr(dat,"description") <- r_data[[paste0(robj,"_descr")]]
        assign(robj, dat)
        save(list = robj, file = file)
      } else {
        assign(robj, .getdata())
        save(list = robj, file = file)
      }
    } else if(ext == 'csv') {
      assign(robj, .getdata())
      write.csv(get(robj), file)
    }
  }
)

# loading data
observe({
  # loading files from disk
  inFile <- input$uploadfile
  if(!is.null(inFile)) {
    isolate({
      # iterating through the files to upload
      for(i in 1:(dim(inFile)[1]))
        loadUserData(inFile[i,'name'], inFile[i,'datapath'], input$dataType,
                     header = input$header,
                     man_str_as_factor = input$man_str_as_factor,
                     sep = input$sep)

      updateSelectInput(session, "dataset", label = "Datasets:",
                        choices = r_data$datasetlist,
                        selected = r_data$datasetlist[1])
    })
  }
})

# loading all examples files (linked to helpfiles)
observe({
  if(not_pressed(input$loadExampleData)) return()
  isolate({

    # loading data bundled with Radiant
    # path <- "data/data_examples/"
    # path <- "../../data/"
    path <- "../base/data/"
    examples <- list.files(path)

    for(ex in examples) loadUserData(ex, paste0(path,ex), 'rda')

    # loading data available for Rady students
    # path <- "data/data_rady/"
    path <- "data/"
    examples <- list.files(path)

    for(ex in examples) loadUserData(ex, paste0(path,ex), 'rda')

    # sorting files alphabetically
    r_data[['datasetlist']] <- sort(r_data[['datasetlist']])

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist,
                      selected = r_data$datasetlist[1])
  })
})

observe({
  # 'reading' data from clipboard
  if(not_pressed(input$loadClipData)) return()
  isolate({
    loadClipboardData()
    updateRadioButtons(session = session, inputId = "dataType",
                       label = "Load data:",
                       c("rda" = "rda", "csv" = "csv", "clipboard" = "clipboard",
                         "examples" = "examples", "state" = "state"),
                       selected = "rda", inline = TRUE)
    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist, selected = "xls_data")
  })
})

#######################################
# Load previous state
#######################################
observe({
  inFile <- input$uploadState
  if(!is.null(inFile)) {
    isolate({
      tmpEnv <- new.env()
      load(inFile$datapath, envir=tmpEnv)
      if (exists("r_data", envir=tmpEnv, inherits=FALSE))
        assign(ip_data, tmpEnv$r_data, envir=.GlobalEnv)
      if (exists("r_state", envir=tmpEnv, inherits=FALSE))
        assign(ip_inputs, tmpEnv$r_state, envir=.GlobalEnv)
      assign(ip_dump, now(), envir = .GlobalEnv)
      rm(tmpEnv)
    })
  }
})

output$refreshOnUpload <- renderUI({
  inFile <- input$uploadState
  if(!is.null(inFile)) {
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

output$downloadState <- downloadHandler(
  filename = function() { paste0("RadiantState-",Sys.Date(),".rda") },
  content = function(file) {
    saveState(file)
  }
)

#######################################
# Loading data into memory
#######################################
observe({
  if(input$data_rename %>% is_empty) return()
  if(input$renameButton %>% not_pressed) return()
  isolate({
    # you can use pryr::object_size to see that the size of the list doesn't change
    # when you assign a list element another name
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
  # Drop-down selection of data set
  tagList(
    selectInput(inputId = "dataset", label = "Datasets:", choices = r_data$datasetlist,
      selected = state_init("dataset"), multiple = FALSE),
    conditionalPanel(condition = "input.datatabs == 'Manage'",
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
  list(
    textInput("data_rename", "", input$dataset),
    actionButton('renameButton', 'Rename')
  )
})

output$htmlDataExample <- renderText({

  # dat <- .getdata()
  if(is.null(.getdata())) return()

  # Show only the first 10 (or 30) rows
  r_data[[paste0(input$dataset,"_descr")]] %>%
    { is_empty(.) %>% ifelse(., 30, 10) } %>%
    show_data_snippet(nshow = .)
})
