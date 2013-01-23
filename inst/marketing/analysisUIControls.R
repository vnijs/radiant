# variable selection - hclustering
output$hc_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

output$km_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "km_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})
  
ui_hclustering <- function() {
  wellPanel(
    uiOutput("hc_vars"), tags$style(type='text/css', "#hc_vars { height: 250px; padding-bottom: 35px;}"),
    selectInput(inputId = "hc_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("hc_saveclus", "Save cluster membership:")
  )
}

ui_kmeansClustering <- function() {
  wellPanel(
    uiOutput("km_vars"), tags$style(type='text/css', "#km_vars { height: 250px; padding-bottom: 35px;}"),
    numericInput("km_seed", "Set random seed:", 1234, min = 0),
    selectInput(inputId = "km_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("km_saveclus", "Save cluster membership")
  )
}

# the 'grand' ui-element caller
output$analysis_ui_controls <- reactiveUI(function() {
  if(input$tool == "dataview") return()
  get(paste('ui_',input$tool, sep=""))()
})

