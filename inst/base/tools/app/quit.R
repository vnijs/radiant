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
        checkboxInput('show_input', 'Show input', FALSE),
        checkboxInput('show_data', 'Show r_data', FALSE),
        checkboxInput('show_state', 'Show state', FALSE)),

      help_modal('Quit','quit_help',inclMD(file.path(r_path,"base/tools/help/quit.md")))
    ),
    mainPanel(
      # verbatimTextOutput("show_session"),
      conditionalPanel(condition = "input.show_input == true",
        verbatimTextOutput("show_input")
      ),
      conditionalPanel(condition = "input.show_data == true",
        verbatimTextOutput("show_data")
      ),
      conditionalPanel(condition = "input.show_state == true",
        verbatimTextOutput("show_state")
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

# observe({
#   updateTabsetPanel(session, "nav_radiant", selected = "Quit")
# })

# output$show_session <- renderPrint({
#   cat("Session list:\n")
#   reactiveValuesToList(session$clientData)
# })

output$show_input <- renderPrint({
  input$show_input # only update when you toggle the checkbox
  isolate({
    cat("Input list:\n")
    inp <- reactiveValuesToList(input)
    str(inp[sort(names(inp))])
  })
})

output$show_data <- renderPrint({
  input$show_data # only update when you toggle the checkbox
  isolate({
    cat("r_data list:\n")
    reactiveValuesToList(r_data) %>%
      {str(.[sort(names(.))])}
  })
})

output$show_state <- renderPrint({
  cat("State list:\n")
  if (is.null(r_state)) return()
  if (length(r_state) == 0) return("[empty]")
  str(r_state[sort(names(r_state))])
})

observe({
  if (not_pressed(input$quitApp) || !r_local) return()

  # quit R, unless you are running an interactive session
  if (interactive()) {
    # flush input and r_data into Rgui or Rstudio
    isolate({
      assign("r_state", reactiveValuesToList(input), envir = .GlobalEnv)
      assign("r_data", reactiveValuesToList(r_data), envir = .GlobalEnv)
      stop_message <- "\nStopping Radiant. State available as 'r_state' and 'r_data'.\n"

      if (!is_empty(input$rmd_report)) {

        "```{r echo = FALSE}\n# knitr::opts_chunk$set(comment=NA, cache=FALSE, message=FALSE, warning=FALSE)\n# suppressMessages(library(radiant))\n# uncomment the lines above to 'knit' the Rmd file in Rstudio\n# you will also need to load the data using load()\n```\n\n" %>%
          paste0(.,input$rmd_report) -> rmd_report
        os_type <- Sys.info()["sysname"]
        if (os_type == 'Windows') {
          cat(rmd_report, file = "clipboard")
          stop_message %<>% paste0(., "Report content was copied to the clipboard.\n")
        } else if (os_type == "Darwin") {
          cat(rmd_report, file = pipe("pbcopy"))
          stop_message %<>% paste0(., "Report content was copied to the clipboard.\n")
        } else if (os_type == "Linux") {
          # cat(input$rmd_report, file = pipe("pbcopy"))
        }
      }
      try(rm(r_env, envir = .GlobalEnv), silent = TRUE) # removing the reference to the shiny environment
      try(rm(r_sessions, envir = .GlobalEnv), silent = TRUE) # removing r_sessions
      try(unlink("~/radiant_figures/", recursive = TRUE), silent = TRUE) # removing temp knitr figures directory
      cat(stop_message)
      stopApp("-- stop successful --")
    })
  } else {
    stopApp("Stopped Radiant")
    q("no")
  }
})
