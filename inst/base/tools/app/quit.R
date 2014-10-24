#######################################
# Quit menu
#######################################
output$savequit <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML("<label>Save app state:</label>"),
        downloadButton('downloadStateQuit', 'Save')
      ),
      wellPanel(
        HTML("<label>Reset app state:</label>"),
        HTML("<button id='resetState' type='button' class='btn action-button' onClick='window.location.reload()'>Reset</button></br>"),
        checkboxInput('showInput', 'Show input', FALSE), br(),
        checkboxInput('showState', 'Show state', FALSE)
      ),
      wellPanel(
        HTML("<label>Quit:</label>"),
        actionButton('quitApp', 'Quit')
      ),
      helpModal('State','stateHelp',inclMD("../base/tools/help/state.md"))
    ),
    mainPanel(
      conditionalPanel(condition = "input.showInput == true",
        verbatimTextOutput("showInput")
      ),
      conditionalPanel(condition = "input.showState == true",
        verbatimTextOutput("showState")
      )
    )
  )
})

output$downloadStateQuit <- downloadHandler(
  filename = function() { paste0("RadiantState-",Sys.Date(),".rsf") },
  content = function(file) {

    isolate({
      RadiantInputs <- state_list
      LiveInputs <- reactiveValuesToList(input)
      RadiantInputs[names(LiveInputs)] <- LiveInputs
      RadiantValues <- reactiveValuesToList(values)
      save(RadiantInputs, RadiantValues , file = file)

#       RadiantInputs <- isolate(reactiveValuesToList(input))
#       RadiantValues <- isolate(reactiveValuesToList(values))
#       save(RadiantInputs, RadiantValues , file = file)
    })
  }
)

output$showInput <- renderPrint({
  isolate({
    cat("Input list:\n")
    inp <- reactiveValuesToList(input)
    str(inp[sort(names(inp))])
  })
})

output$showState <- renderPrint({
  cat("State list:\n")
  str(state_list[sort(names(state_list))])
})

observe({
  if(is.null(input$resetState) || input$resetState == 0) return()
  # cleaning out the state file temp
  pth <- "~/radiant_temp/state/"
  # if(file.exists(pth)) unlink(pth, recursive = TRUE)
  filename = paste0(pth,"RadiantState-",Sys.Date(),".rsf")
  if(file.exists(filename)) file.remove(filename)
  setInitValues()
})

observe({
  if(is.null(input$quitApp) || input$quitApp == 0) return()
  pth <- "~/radiant_temp/rmd/figure/"
  if(file.exists(pth)) unlink(pth, recursive = TRUE)
  pth <- "~/radiant_temp/state/"
  filename = paste0(pth,"RadiantState-",Sys.Date(),".rsf")
  if(file.exists(filename)) file.remove(filename)

  # quit R, unless you are running Rstudio or Rgui
  if(Sys.getenv("RSTUDIO") != "1") {
    stopApp("Stopped Radiant")   # stop Radiant
    q("no")
  } else {
    # flush input and values into Rstudio
    rret <<- list()
    rret$input <<- isolate(reactiveValuesToList(input))
    rret$values <<- isolate(reactiveValuesToList(values))
    stopApp("Stopping Radiant. Input and Values returned in list rret") # stop Radiant
  }
})
