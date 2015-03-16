#######################################
# Quit menu
#######################################
output$savequit <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        downloadButton('downloadStateQuit', 'Save state')),
      wellPanel(
        tags$button(id = 'resetState', type = "button",
                    class = "btn btn-default action-button shiny-bound-input",
                    onclick = "window.location.reload();",
                    "Reset state")),
      wellPanel(
        tags$button(id = 'quitApp', type = "button",
                    class = "btn btn-default action-button shiny-bound-input",
                    onclick = "window.close();",
                    "Quit")),
      wellPanel(
        checkboxInput('showInput', 'Show input', FALSE),
        checkboxInput('showState', 'Show state', FALSE)),

      help_modal('Quit','quitHelp',inclMD("../base/tools/help/quit.md"))
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
  filename = function() { paste0("RadiantState-",Sys.Date(),".rda") },
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
  if(not_pressed(input$quitApp) || !running_local) return()

  # quit R, unless you are running an interactive session
  if(interactive()) {
    # flush input and r_data into Rgui or Rstudio
    isolate({
      assign("r_state", reactiveValuesToList(input), envir = .GlobalEnv)
      assign("r_data", reactiveValuesToList(r_data), envir = .GlobalEnv)
      stop_message <- "\nStopping Radiant. State available as 'r_state' and 'r_data'.\n"
      # if(!is.null(input$rmd_report) && input$rmd_report != "") {
      if(!is_empty(input$rmd_report)) {
        os_type <- Sys.info()["sysname"]
        if (os_type == 'Windows') {
          cat(input$rmd_report, file = "clipboard")
          stop_message %<>% paste0(., "Report content was copied to the clipboard.\n")
        } else if (os_type == "Darwin") {
          cat(input$rmd_report, file = pipe("pbcopy"))
          stop_message %<>% paste0(., "Report content was copied to the clipboard.\n")
        } else if (os_type == "Linux") {
          # cat(input$rmd_report, file = pipe("pbcopy"))
        }
      }
      rm(r_env, envir = .GlobalEnv) # removing the reference to the shiny environment
      stopApp(cat(stop_message))
    })
  } else {
    stopApp("Stopped Radiant")
    q("no")
  }
})
