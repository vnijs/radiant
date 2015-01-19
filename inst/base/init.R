################################################################################
# functions to set initial values and take information from state_list
# when available
################################################################################

ip <- session$request$REMOTE_ADDR

init_state <- function() {

  # initial plot height and width
  values$plotHeight <<- 600
  values$plotWidth <<- 600

  # Datasets can change over time (i.e. the changedata function). Therefore,
  # the data need to be a reactive value so the other reactive functions
  # and outputs that depend on these datasets will know when they are changed.
  robj <- load("../base/data/data_init/diamonds.rda")
  df <- get(robj)
  values[["diamonds"]] <<- df
  values[["diamonds_descr"]] <<- attr(df,'description')
  values$datasetlist <<- c("diamonds")
}

ip_inputs <- paste0("RadiantInputs",ip)
ip_values <- paste0("RadiantValues",ip)

if (exists("state_list") && exists("values")) {
  values <- do.call(reactiveValues, values)
  state_list <- state_list
  rm(values, state_list, envir = .GlobalEnv)
} else if (exists(ip_inputs) && exists(ip_values)) {
  values <- do.call(reactiveValues, get(ip_values))
  state_list <- get(ip_inputs)
  rm(list = c(ip_inputs, ip_values), envir = .GlobalEnv)
} else {
  state_list <- list()
  values <- reactiveValues()
  init_state()
}

observe({
  # reset state_list on dataset change
  if(is.null(state_list$dataset) || is.null(input$dataset)) return()
  if(state_list$dataset != input$dataset) state_list <<- list()
})
