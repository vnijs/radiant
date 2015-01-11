################################################################################
# functions to set initial values and take information from state_list
# when available
################################################################################

state_singlevar <- function(inputvar, vars) vars[vars == state_list[[inputvar]]]
state_multvar <- function(inputvar, vars) vars[vars %in% state_list[[inputvar]]]

state_init <- function(inputvar, init = "") {
  ifelse(is.null(state_list[[inputvar]]), return(init), return(state_list[[inputvar]]))
}

state_init_list <- function(inputvar, init = "", vals) {
  ifelse(is.null(state_list[[inputvar]]), return(init), return(state_singlevar(inputvar, vals)))
}

state_init_multvar <- function(inputvar, pre_inputvar, vals) {
  # for factor and cluster use variable selection from the pre-analysis 
  ifelse(is.null(state_list[[inputvar]]), return(vals[vals %in% pre_inputvar]),
    return(state_multvar(inputvar, vals)))
}

#######################################
# Load, Save, Reset app state
#######################################
output$state <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML("<label>Load previous app state:</label>"),
        fileInput('uploadState', ''),
        uiOutput("refreshOnUpload")
      ),
      wellPanel(
        HTML("<label>Save current app state:</label>"),
        downloadButton('downloadState', 'Save')

      ),
      wellPanel(
        HTML("<label>Reset to initial app state:</label>"),
        HTML("<a class='btn action-button shiny-bound-input' id='resetState' type='button' href='/'>Reset</a>")
      ),
      wellPanel(
        HTML("<label>Quit app:</label>"),
        actionButton('quitApp', 'Quit')
      ),
      helpModal('State','stateHelp',inclMD("tools/help/state.md"))
    ),
    mainPanel(
      verbatimTextOutput("showInput")
    )
  )
})

observe({
  inFile <- input$uploadState
  if(!is.null(inFile)) {
    isolate({
      load(inFile$datapath)
      if(exists("RadiantValues")) values <<- do.call(reactiveValues, RadiantValues)
      if(exists("RadiantInputs")) state_list <<- RadiantInputs
    })
  }
})

output$refreshOnUpload <- renderUI({
  inFile <- input$uploadState
  if(!is.null(inFile)) {
    # Joe Cheng: https://groups.google.com/forum/#!topic/shiny-discuss/Olr8m0JwMTo
    tags$script("window.location.reload();")

    #################################################################################
    # todo: return to the state page or the page used before going to State. 
    # The below doesn't work ... yet
    #################################################################################
    # updateTabsetPanel(session, "nav_radiant", selected = "State") 
  }
})

output$downloadState <- downloadHandler(
  filename = function() { paste0("RadiantState-",Sys.Date(),".rda") },
  content = function(file) {

    isolate({
      RadiantInputs <- isolate(reactiveValuesToList(input))
      RadiantValues <- isolate(reactiveValuesToList(values))
      save(RadiantInputs, RadiantValues , file = file)
    })
  }
)

observe({
  if(is.null(input$resetState) || input$resetState == 0) return()
  setInitValues()
})

observe({
  if(is.null(input$quitApp) || input$quitApp == 0) return()
  unlink(c("www/rmd/cache/","www/rmd/figure"), recursive = TRUE)
  stopApp()   # stop Radiant
  q("no")     # quit R
})

output$showInput <- renderPrint({
  cat("State list:\n")
  print(state_list[sort(names(state_list))])
})
