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
        # actionButton('resetState', 'Reset'),
        # uiOutput("refreshOnReset"),
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
  # query <- parseQueryString(session$clientData$url_search)

  # # Return a string with key-value pairs
  # print(query)
  # print(str(query))
  # # paste(names(query), query, sep = "=", collapse=", ") %>% cat

  # cat("\n\n")


  # cat("Env?\n")
  # req <- ls(env=session$request)
  # for(i in req)
  #   print(get(i, envir=session$request))

  # cat("Env?\n")

  # cdata <- session$clientData
  # # cdata <- session$request

  # # Values from cdata returned as text
  # cnames <- names(cdata)

  # allvalues <- lapply(cnames, function(name) {
  #   paste(name, cdata[[name]], sep=" = ")
  # })
  # paste(allvalues, collapse = "\n") %>% cat

  # paste(sep = "",
  #   "\nprotocol: ", session$clientData$url_protocol, "\n",
  #   "hostname: ", session$clientData$url_hostname, "\n",
  #   "pathname: ", session$clientData$url_pathname, "\n",
  #   "port: ",     session$clientData$url_port,     "\n",
  #   "search: ",   session$clientData$url_search,   "\n",
  #   "IP address XFF: ", session$request$HTTP_X_FORWARDED_FOR,   "\n",
  #   "IP address: ", session$request$REMOTE_ADDR,   "\n"
  # ) %>% cat
  # print("ip next")
  # print(ip)
  # print("ip prev")
  # print(ls())
  # print(ls(envir = .GlobalEnv))
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
  if(input$quitApp %>% not_pressed || !running_local) return()

  # quit R, unless you are running Rstudio
  # if(Sys.getenv("RSTUDIO") != "1") {
  if(interactive()) {
    # flush input and values into Rgui or Rstudio
    isolate({
      assign("state_list", reactiveValuesToList(input), envir = .GlobalEnv)
      assign("values", reactiveValuesToList(values), envir = .GlobalEnv)
      if(!is.null(input$rmd_report) && input$rmd_report != "") {
        os_type <- .Platform$OS.type
        if (os_type == 'windows') {
          cat(input$rmd_report, file = "clipboard")
        } else {
          cat(input$rmd_report, file = pipe("pbcopy"))
        }
      }
      stopApp(cat("\nStopping Radiant. State available as 'state_list' and 'values'.\nReport content was copied to the clipboard.\n"))
      rm(env_shiny) # removing the reference to the shiny environment
    })
  } else {
    stopApp("Stopped Radiant")
    q("no")
  }
})
