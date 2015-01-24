#######################################
# Manage datasets in/out of Radiant
#######################################

output$ui_fileUpload <- renderUI({

  if(is.null(input$dataType)) return()
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

output$ui_Manage <- renderUI({
  list(
    wellPanel(
      radioButtons(inputId = "dataType", label = "Load data:",
                   c("rda" = "rda", "csv" = "csv",  "clipboard" = "clipboard",
                     "examples" = "examples", "state" = "state"),
                     selected = "rda"),
      conditionalPanel(condition = "input.dataType != 'clipboard' &&
                                    input.dataType != 'examples'",
        conditionalPanel(condition = "input.dataType == 'csv'",
          checkboxInput('header', 'Header', TRUE),
          checkboxInput('man_str_as_factor', 'String as Factor', TRUE),
          radioButtons('sep', '', c(Comma=',', Semicolon=';', Tab='\t'), ',')
        ),
        uiOutput("ui_fileUpload")
      ),
      conditionalPanel(condition = "input.dataType == 'clipboard'",
        actionButton('loadClipData', 'Paste data')
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
                     "state" = "state"), selected = "rda"),

      conditionalPanel(condition = "input.saveAs == 'clipboard'",
        actionButton('saveClipData', 'Copy data')
      ),
      conditionalPanel(condition = "input.saveAs != 'clipboard' &&
                                    input.saveAs != 'state'",
        downloadButton('downloadData', 'Save')
      ),
      conditionalPanel(condition = "input.saveAs == 'state'",
        HTML("<label>Save current app state:</label>"),
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
    helpModal('Manage','manageHelp',inclMD("../base/tools/help/manage.md"))
  )
})

# updating the dataset description
observe({
  if(is.null(input$updateDescr) || input$updateDescr == 0) return()
  isolate({
    values[[paste0(input$dataset,"_descr")]] <- input$man_data_descr
    updateCheckboxInput(session = session, "man_add_descr",
                        "Add/edit data description", FALSE)
  })
})

output$dataDescriptionHTML <- renderUI({

  isolate(values[[paste0(input$dataset,"_descr")]]) %>%
    descr_out('html') %>%
    HTML
})

output$dataDescriptionMD <- renderUI({
  tagList(
    "<label>Add data description:</label>" %>% HTML,
    tags$textarea(id="man_data_descr",
                  rows="15",
                  style="width:650px;",
                  descr_out(
                    isolate(values[[paste0(input$dataset,"_descr")]]),
                    'md'
                  )
    )
  )
})



# removing datasets
output$uiRemoveDataset <- renderUI({
  # Drop-down selection of data set to remove
  selectInput(inputId = "removeDataset", label = "",
    choices = values$datasetlist, selected = NULL, multiple = TRUE,
    selectize = FALSE
  )
})

observe({
  # removing datasets
  if(is.null(input$removeDataButton) || input$removeDataButton == 0) return()
  isolate({

    # only remove datasets if 1 or more were selected
    # without this line all files would be removed when the removeDataButton
    # is pressed
    if(is.null(input$removeDataset)) return()
    datasets <- values[['datasetlist']]
    if(length(datasets) > 1) {  # have to leave at least one dataset
      removeDataset <- input$removeDataset
      if(length(datasets) == length(removeDataset))
        removeDataset <- removeDataset[-1]

      # Must use single string to index into reactivevalues so loop is necessary
      for(rem in removeDataset) {
        values[[rem]] <- NULL
        values[[paste0(rem,"_descr")]] <- NULL
      }
      values[['datasetlist']] <- datasets[-which(datasets %in% removeDataset)]
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
                       c("rda" = "rda", "csv" = "csv",
                         "clipboard" = "clipboard", "state" = "state"),
                       selected = "rda")
  })
})

output$downloadData <- downloadHandler(
  filename = function() { paste(input$dataset,'.',input$saveAs, sep='') },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$dataset

    if(ext == 'rda') {
      if(!is.null(input$man_data_descr) && input$man_data_descr != "") {
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
  # if(!is.null(inFile) && !is.na(inFile)) {
  if(!is.null(inFile)) {
    isolate({
      # iterating through the files to upload
      for(i in 1:(dim(inFile)[1]))
        loadUserData(inFile[i,'name'], inFile[i,'datapath'], input$dataType,
                     header = input$header,
                     man_str_as_factor = input$man_str_as_factor,
                     sep = input$sep)

      # loadUserData <- function(filename, uFile, ext, header = TRUE,
      #                    man_str_as_factor = TRUE, sep = ",") {

      updateSelectInput(session, "dataset", label = "Datasets:",
                        choices = values$datasetlist,
                        selected = values$datasetlist[1])
    })
  }
})

# loading all examples files (linked to helpfiles)
observe({
  if(input$loadExampleData %>% not_pressed) return()
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

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = values$datasetlist,
                      selected = values$datasetlist[1])
  })
})

observe({
  # 'reading' data from clipboard
  if(is.null(input$loadClipData) || input$loadClipData == 0) return()
  isolate({
    loadClipboardData()
    updateRadioButtons(session = session, inputId = "dataType",
                       label = "Load data:", c("rda" = "rda", "csv" = "csv",
                                               "clipboard" = "clipboard",
                                               "examples" = "examples",
                                               "state" = "state"),
                       selected = "rda")

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = values$datasetlist, selected = 'xls_data')
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
      if (exists("values", envir=tmpEnv, inherits=FALSE))
        assign(ip_values, tmpEnv$values, envir=.GlobalEnv)
      if (exists("state_list", envir=tmpEnv, inherits=FALSE))
        assign(ip_inputs, tmpEnv$state_list, envir=.GlobalEnv)
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
    state_list[names(LiveInputs)] <- LiveInputs
    values <- reactiveValuesToList(values)
    save(state_list, values , file = filename)
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
  if(is.null(input$data_rename)) return()
  if(is.null(input$renameButton) || input$renameButton == 0) return()
  isolate({
    values[[input$data_rename]] <- values[[input$dataset]]
    values[[input$dataset]] <- NULL
    values[[paste0(input$data_rename,"_descr")]] <- values[[paste0(input$dataset,"_descr")]]
    values[[paste0(input$dataset,"_descr")]] <- NULL

    ind <- which(input$dataset == values[['datasetlist']])
    values[['datasetlist']][ind] <- input$data_rename

    updateSelectInput(session, "dataset", label = "Datasets:", choices = values$datasetlist,
                      selected = input$data_rename)
  })
})

output$uiDatasets <- renderUI({
  # Drop-down selection of data set
  list(wellPanel(
    selectInput(inputId = "dataset", label = "Datasets:", choices = values$datasetlist,
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
  ))
})

output$uiRename <- renderUI({
  list(
    textInput("data_rename", "", input$dataset),
    actionButton('renameButton', 'Rename')
  )
})

output$htmlDataExample <- renderText({

  dat <- getdata()
  if(is.null(dat)) return()

  # Show only the first 10 (or 30) rows
  descr <- values[[paste0(input$dataset,"_descr")]]
  nshow <- 10
  if(is.null(descr) || descr == "") nshow <- 30

  show_data_snippet(nshow = nshow)
})
