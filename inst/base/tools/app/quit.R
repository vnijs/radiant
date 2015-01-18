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
      # , uiOutput("debugTools")
    )
  )
})

output$downloadStateQuit <- downloadHandler(
  filename = function() { paste0("RadiantState-",Sys.Date(),".rsf") },
  content = function(file) {
    saveState(file)
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
  if(is.null(state_list)) return()
  if(length(state_list) == 0) return("[empty]")
  str(state_list[sort(names(state_list))])
})

observe({
#   if(is.null(input$resetState) || input$resetState == 0) return()
  if(input$resetState %>% not_pressed) return()
  # cleaning out the state file temp
  unlink("~/radiant_temp/state/RadiantState*.rsf")

#   state_list <<- list()
#   values <<- reactiveValues()
#
#   paste0("state_list",session$request$REMOTE_ADDR) %>%
#     assign(., NULL, envir = .GlobalEnv)
#   paste0("values",session$request$REMOTE_ADDR) %>%
#     assign(., NULL, envir = .GlobalEnv)

  setInitValues()
})

observe({
  if(is.null(input$quitApp) || input$quitApp == 0) return()
  unlink("~/radiant_temp/rmd/figure/*", recursive = TRUE)
  unlink("~/radiant_temp/state/RadiantState*.rsf")

  # quit R, unless you are running Rstudio or Rgui
  if(Sys.getenv("RSTUDIO") != "1") {
    stopApp("Stopped Radiant")   # stop Radiant
    q("no")
  } else {
    # flush input and values into Rstudio
    isolate({
      radiant <<- list()
      radiant$input <<- isolate(reactiveValuesToList(input))
      radiant$values <<- isolate(reactiveValuesToList(values))
      stopApp("Stopping Radiant. Input and Values returned in list radiant") # stop Radiant
    })
  }
})
