# How much of this could/should be separated out into a separate tool?

################################################################
# functions used in radyant
################################################################
varnames <- function() {
  if(is.null(input$datasets)) return()

  dat <- getdata()
  cols <- colnames(dat)
  names(cols) <- paste(cols, " {", sapply(dat,class), "}", sep = "")
  cols
}

changedata <- function(addCol = NULL, addColName = "") {
  # function that changes data as needed
  if(is.null(addCol) || addColName == "") return()
  # We don't want to take a reactive dependency on anything
  isolate({
    values[[input$datasets]][,addColName] <- addCol
  })
}

getdata <- function(dataset = input$datasets) {
  values[[dataset]]
} 

loadUserData <- function(uFile) {

  ext <- file_ext(uFile)
  objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(uFile))
  ext <- tolower(ext)

  if(ext == 'rda' || ext == 'rdata') {
    # objname will hold the name of the object inside the R datafile
    objname <- robjname <- load(uFile)
    values[[robjname]] <- get(robjname)
  }

  if(datasets[1] == '') {
    datasets <<- c(objname)
  } else {
    datasets <<- unique(c(objname,datasets))
  }

  if(ext == 'sav') {
    values[[objname]] <- read.sav(uFile)
  } else if(ext == 'dta') {
    values[[objname]] <- read.dta(uFile)
  } else if(ext == 'csv') {
    values[[objname]] <- read.csv(uFile)
  }
}

loadPackData <- function(pFile) {

  robjname <- data(list = pFile)
  dat <- get(robjname)

  if(pFile != robjname) return("R-object not found. Please choose another dataset")

  if(is.null(ncol(dat))) {
    # values[[packDataSets]] <- packDataSets[-which(packDataSets == pFile)]
    return()
  }

  values[[robjname]] <- dat

  if(datasets[1] == '') {
    datasets <<- c(robjname)
  } else {
    datasets <<- unique(c(robjname,datasets))
  }
}

#################################################
# reactive functions used in radyant
#################################################

uploadfunc <- reactive(function() {
  if(input$upload == 0) return("")
  fpath <- try(file.choose(), silent = TRUE)
  if(is(fpath, 'try-error')) {
    return("")
  } else {
    return(fpath)
  }
})

output$downloadData <- downloadHandler(
  filename = function() { paste(input$datasets[1],'.',input$saveAs, sep='') },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$datasets[1]
    assign(robj, getdata())

    if(ext == 'rda' || ext == 'rdata') {
      save(list = robj, file = file)
    } 
    else if(ext == 'dta') {
      write.dta(get(robj), file)
    } else if(ext == 'csv') {
      write.csv(get(robj), file)
    }
  }
)

output$datasets <- reactiveUI(function() {

  fpath <- uploadfunc()
  # loading user data
  if(fpath != "") loadUserData(fpath)

  # loading package data
  if(input$packData != "") {
    if(input$packData != lastLoaded) {
      loadPackData(input$packData)
      lastLoaded <<- input$packData 
    }
  }
  # Drop-down selection of data set
  selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
})

output$columns <- reactiveUI(function() {
  cols <- varnames()
  selectInput("columns", "Select columns to show:", choices  = as.list(cols), selected = names(cols), multiple = TRUE)
})

output$nrRows <- reactiveUI(function() {
  if(is.null(input$datasets)) return()
  dat <- getdata()

  # number of observations to show in dataview
  nr <- nrow(dat)
  sliderInput("nrRows", "Rows to show (max 50):", min = 1, max = nr, value = min(15,nr), step = 1)
})

################################################################
# Data reactives - view, plot, transform data, and log your work
################################################################
output$dataviewer <- reactiveTable(function() {
  if(is.null(input$datasets) || is.null(input$columns)) return()

  dat <- getdata()

  # not sure why this is needed when files change ... but it is
  # without it you will get errors the invalid columns have been
  # selected
  if(!all(input$columns %in% colnames(dat))) return()

  # Show only the selected columns and no more than 50 rows
  # at a time
  nr <- input$nrRows
  data.frame(dat[max(1,nr-50):nr, input$columns, drop = FALSE])

  # idea: Add download button so data can be saved
  # example here https://github.com/smjenness/Shiny/blob/master/SIR/server.R
})

################################################################
# Output controls for the Summary, Plots, and Extra tabs
# The tabs are re-used for various tools. Depending on the tool
# selected by the user the appropropriate analaysis function 
# is called.
# Naming conventions: The reactive function to be put in the
# code block above must be of the same name as the tool
# in the tools drop-down. See global.R for the current list
# of tools (and tool-names) 
################################################################

# Generate output for the summary tab
output$summary <- reactivePrint(function() {
  if(is.null(input$datasets) || input$tool == 'dataview') return()

  # get the summary function for currenly selected tool and feed
  # it the output from one of the analysis reactives above
  # get-function structure is used because there may be a large
  # set of tools that will have the same output structure
  f <- get(paste("summary",input$tool,sep = '.'))
  result <- get(input$tool)()
  if(is.character(result)) {
    cat(result,"\n")
  } else {
    f(result)
  }
})

# Generate output for the plots tab
output$plots <- reactivePlot(function() {

  # plotting could be expensive so only done when tab is being viewed
  if(input$tool == 'dataview' || input$analysistabs != 'Plots') return()

  f <- get(paste("plot",input$tool,sep = '.'))
  result <- get(input$tool)()
  if(!is.character(result)) {
    f(result)
  } else {
    plot(x = 1, type = 'n', main="No variable selection made", axes = FALSE, xlab = "", ylab = "")
  }
}, width=600, height=600)



# summary_panel <- function(tp = tabPanel("Summary", verbatimTextOutput("summary"))) {
#   tp
# }
shinyUI(

  pageWithSidebar(

    # headerPanel("Radyant - Marketing analytics using Shiny"),
    headerPanel(''),

    sidebarPanel(

      includeHTML("www/navbar.html"),
      includeHTML("www/js/tools.js"),
      includeHTML('www/js/lr.js'), # needed for livereload

      wellPanel(
        # if there are no datasets available only show the UI to make data available
        conditionalPanel(condition = "input.datasets != ''",
          includeHTML('www/tools.html')
        ),
        uiOutput("datasets")
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'dataview'",
        conditionalPanel(condition = "input.datatabs == 'Data view'",
          wellPanel(
            HTML("<label>Load user data: (.rda | .csv | .sav | .dta)</label>"),
            actionButton("upload", "Choose a file"),
            # helpText("Loading user data disabled on Shiny-Server"),
            br(), br(),
            selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
          )
        ),
        conditionalPanel(condition = "input.datatabs == 'Data view' && input.datasets != ''",
          wellPanel(
            uiOutput("nrRows"), 
            uiOutput("columns"), 
            tags$style(type='text/css', "#columns { height: 200px; padding-bottom: 35px;}")
          )
        ),
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
          uiOutput("ui_visualize")
        ),
        conditionalPanel(condition = "input.datatabs == 'Transform'",
          uiOutput("ui_transform")
        )
      ),
      conditionalPanel(condition = "input.tool != 'dataview'",
        # the appropriate analysis tool is called based on selection from
        # tools dropdown
        uiOutput("ui_analysis")
        # submitButton("Test")
      )
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.datasets != ''",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", 
              selectInput("saveAs", "", choices = c('rda','csv','dta'), selected = NULL, multiple = FALSE),
              tags$style(type='text/css', "#saveAs { width: 85px;}"),
              downloadButton('downloadData', 'Save data'),
              tags$style(type='text/css', "#downloadData { vertical-align: top; width: 70px;}"),
              # tags$style(type='text/css', "#downloadData { padding-bottom: 35px;}"),
              tableOutput("dataviewer")
            ),
            tabPanel("Visualize", plotOutput("visualize", height = "100%")),
            tabPanel("Transform", 
              tableOutput("transform_data"), br(),
              tableOutput("transform_summary")
            ),
            tabPanel("About", includeMarkdown("about.md"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Summary", verbatimTextOutput("summary")),
            # uiOutput("summary_panel"),
            # summary_panel(),
            tabPanel("Plots", plotOutput("plots", height = "100%")),
            tabPanel("Log", verbatimTextOutput('logwork'))
          )
        )
      )
    )
  )
)