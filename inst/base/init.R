################################################################################
# functions to set initial values and take information from state_list
# when available
################################################################################

# using a function here so it can also be called from quit.R to reset the app
setInitValues <- function() {
  # initialize state list and reactive values
  pth <- "~/radiant_temp/state"
  filename = paste0(pth,"/RadiantState-",Sys.Date(),".rsf")
  if(file.exists(filename)) {
    load(filename)
    if(exists("RadiantValues")) values <<- do.call(reactiveValues, RadiantValues)
    if(exists("RadiantInputs")) state_list <<- RadiantInputs
    backup_loaded <<- TRUE
  } else {

    backup_loaded <<- FALSE

    # different proc for server?
    # if(running_local)

    state_list <<- list()
    values <<- reactiveValues()

    # initial plot height and width
    values$plotHeight <- 600
    values$plotWidth <- 600

    # Datasets can change over time (i.e. the changedata function). Therefore,
    # the data need to be a reactive value so the other reactive functions
    # and outputs that depend on these datasets will know when they are changed.
    robj <- load("../base/data/data_init/diamonds.rda")
    df <- get(robj)
    values[["diamonds"]] <- df
    values[["diamonds_descr"]] <- attr(df,'description')
    values$datasetlist <- c("diamonds")
  }
}

# setup reactiveValues object to store data
setInitValues()
