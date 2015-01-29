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
        tags$button(id = 'quitApp', type = "button",
                    class = "btn action-button",
                    onclick = "window.close();",
                    "Quit")
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
  if(is.null(r_state)) return()
  if(length(r_state) == 0) return("[empty]")
  str(r_state[sort(names(r_state))])
})

observe({
  if(input$quitApp %>% not_pressed || !running_local) return()

  # quit R, unless you are running an interactive session
  if(interactive()) {
    # flush input and r_data into Rgui or Rstudio
    isolate({
      assign("r_state", reactiveValuesToList(input), envir = .GlobalEnv)
      assign("r_data", reactiveValuesToList(r_data), envir = .GlobalEnv)
      if(!is.null(input$rmd_report) && input$rmd_report != "") {
        os_type <- .Platform$OS.type
        if (os_type == 'windows') {
          cat(input$rmd_report, file = "clipboard")
        } else {
          cat(input$rmd_report, file = pipe("pbcopy"))
        }
      }
      rm(r_env, envir = .GlobalEnv) # removing the reference to the shiny environment
      stopApp(cat("\nStopping Radiant. State available as 'r_state' and 'r_data'.\nReport content was copied to the clipboard.\n"))
    })
  } else {
    stopApp("Stopped Radiant")
    q("no")
  }
})
