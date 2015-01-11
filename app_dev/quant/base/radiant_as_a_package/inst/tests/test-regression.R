# initial settings
setwd("~/github/radiant/inst/marketing")
# install.packages('png')
libs <- c("shiny", "knitr", "markdown", "shinyAce", "car", "gridExtra", "psych", 
  "plyr", "reshape2", "vegan", "lubridate", "wordcloud", "AlgDesign", "ggplot2","png")
suppressWarnings(sapply(libs, require, character.only=TRUE))
options(digits = 3)

######################################
# simplest approach without runApp
######################################
input <- list()
output <- list()
values <- list()

robj <- load("data/data_init/diamonds.rda") 
df <- get(robj)
values[["diamonds"]] <- df
values[["diamonds_descr"]] <- attr(df,'description')
values$datasetlist <- c("diamonds")

source("tools/analysis/regression.R")

result <- regression("diamonds", "price", c("carat", "clarity"), NULL, NULL, "none", FALSE, FALSE, FALSE, "scatterlist")

# sink("../tests/regression_no_runApp.txt")
#   summary_regression(result)
# sink()

# works!
res1 <- capture.output(summary_regression(result))
res2 <- paste0(readLines("../tests/regression_correct.txt"))
all.equal(res1,res2)

png('../tests/regression_no_runApp.png')
  plots_regression(result)
dev.off()

# works!
res1 <- readPNG("../tests/regression_no_runApp.png")
res2 <- readPNG("../tests/regression_correct.png")
all.equal(res1,res2)




######################################
# using the runApp(list()) structure
######################################
ui <- basicPage(
  verbatimTextOutput("test_regression")
)

server <- function(session, input, output) {

  source("global.R", local = TRUE)
  source('radiant.R', local = TRUE)
  R.utils::sourceDirectory('tools/analysis', recursive = TRUE)
  R.utils::sourceDirectory('tools/data', recursive = TRUE)
  R.utils::sourceDirectory('tools/app', recursive = TRUE)

  output$test_regression <- renderText({
    result <- regression("diamonds", "price", c("carat", "clarity"), NULL, NULL, "none", FALSE, FALSE, FALSE, "")
    sink("../tests/regression_runApp.txt")
      summary_regression(result)
    sink()
    stopApp()
  })
}

# works!
runApp(list(ui = ui, server = server))
res1 <- paste0(readLines("../tests/regression_runApp.txt"), collapse = "\n")
res2 <- paste0(readLines("../tests/regression_correct.txt"), collapse = "\n")
all.equal(res1,res2)

######################################
# calling reactives directly doesn't work
######################################
ui <- basicPage(
  uiOutput('ui_regression'),
  verbatimTextOutput("test_regression")
)

server <- function(session, input, output) {

  source("global.R", local = TRUE)
  state_list$datasets <<- "diamonds"
  state_list$reg_var1 <<- "price"
  state_list$reg_var2 <<- c("carat","clarity")
  state_list$reg_standardize <<- FALSE

  source('radiant.R', local = TRUE)
  R.utils::sourceDirectory('tools/analysis', recursive = TRUE)
  R.utils::sourceDirectory('tools/data', recursive = TRUE)
  R.utils::sourceDirectory('tools/app', recursive = TRUE)

  output$test_regression <- renderText({

    if(is.null(input$reg_standardize))
      tags$script("window.location.reload();")

    if(is.null(input$reg_var1))
      tags$script("window.location.reload();")

    if(is.null(input$reg_var2))
      tags$script("window.location.reload();")

    print(input$reg_var1)
    print(input$reg_var2)
    print(input$reg_standardize)

    result <- .regression()
    print(result)
    sink("../tests/regression_reactive.txt")
      summary_regression(result)
    sink()
    # if(result != "")
    if(!is.character(result))
      stopApp()
  })
}

# not working input values are NULL and return == ""
runApp(list(ui = ui, server = server))
res1 <- paste0(readLines("../tests/regression_reactive.txt"), collapse = "\n")
res2 <- paste0(readLines("../tests/regression_correct.txt"), collapse = "\n")
all.equal(res1,res2)
state_list
values

######################################
# calling reactives directly doesn't work
# with full ui.R from marketing app
######################################
# source("global.R", local = TRUE)
source("global.R", local = TRUE)

# copy-and-paste from inst/marketing/ui.R
ui <- navbarPage("Radiant", id = "nav_radiant", inverse = TRUE, collapsable = TRUE,

  uiOutput('ui_regression'),

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("Random",
    tabPanel("Sampling and assignment", uiOutput("random")),
    tabPanel("Sample size", uiOutput("sampleSize"))
  ),

  navbarMenu("Base",
    tabPanel("Single mean", uiOutput("singleMean")),
    tabPanel("Compare means", uiOutput("compareMeans")),
    tabPanel("Cross-tabs", uiOutput("crosstab"))
  ),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear (OLS)", uiOutput("regression"))
  ),

  navbarMenu("Maps",
    tabPanel("(Dis)similarity", uiOutput("mds")),
    tabPanel("Attributes", uiOutput("pmap"))
  ),

  navbarMenu("Factor",
    tabPanel("Pre-factor analysis", uiOutput("preFactor")),
    tabPanel("Factor analysis", uiOutput("fullFactor"))
  ),

  navbarMenu("Cluster",
    tabPanel("Hierarchical", uiOutput("hierCluster")),
    tabPanel("K-means", uiOutput("kmeansCluster"))
  ),

  navbarMenu("Conjoint",
    tabPanel("Create profiles", uiOutput("conjointProfiles")),
    tabPanel("Conjoint analysis", uiOutput("conjoint"))
  ),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("State", uiOutput("state")),

  tabPanel("About", includeRmd("tools/app/about.Rmd"))


)

server <- function(session, input, output) {

  # source("global.R", local = TRUE)
  state_list$datasets <<- "diamonds"
  state_list$reg_var1 <<- "price"
  state_list$reg_var2 <<- c("carat","clarity")
  state_list$reg_standardize <<- FALSE

  source('radiant.R', local = TRUE)
  R.utils::sourceDirectory('tools/analysis', recursive = TRUE)
  R.utils::sourceDirectory('tools/data', recursive = TRUE)
  R.utils::sourceDirectory('tools/app', recursive = TRUE)

  output$test_regression <- renderText({

    if(is.null(input$reg_standardize))
      tags$script("window.location.reload();")

    if(is.null(input$reg_var1))
      tags$script("window.location.reload();")

    if(is.null(input$reg_var2))
      tags$script("window.location.reload();")
      
    print(input$reg_var1)
    print(input$reg_var2)
    print(input$reg_standardize)

    result <- .regression()
    print(result)
    sink("../tests/regression_reactive.txt")
      summary_regression(result)
    sink()

    # updateTabsetPanel(session, "nav_radiant", selected = "Report") 


    if(!is.character(result))
      stopApp()
  })
}

# not working input values are NULL and return == ""
runApp(list(ui = ui, server = server))
res1 <- paste0(readLines("../tests/regression_reactive.txt"), collapse = "\n")
res2 <- paste0(readLines("../tests/regression_correct.txt"), collapse = "\n")
all.equal(res1,res2)

res1



######################################
# calling reactives directly doesn't work
# with full ui.R from marketing app
######################################
# source("global.R", local = TRUE)
# source("global.R")

# copy-and-paste from inst/marketing/ui.R
ui <- navbarPage("Radiant", id = "nav_radiant", inverse = TRUE, collapsable = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear (OLS)", uiOutput("regression"))
  ),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("State", uiOutput("state"))
)

server <- function(input, output, session) {

  # source("global.R", local = TRUE)
  source("global.R")
  # state_list$datasets <<- "diamonds"
  # state_list$reg_var1 <<- "price"
  # state_list$reg_var2 <<- c("carat","clarity")
  # state_list$reg_standardize <<- FALSE

  source('radiant.R', local = TRUE)
  R.utils::sourceDirectory('tools/analysis', recursive = TRUE)
  R.utils::sourceDirectory('tools/data', recursive = TRUE)
  R.utils::sourceDirectory('tools/app', recursive = TRUE)

}

# app doesn't work when launched using list() in runApp
runApp(list(ui = ui, server = server))

# trial from R > Code, doesn't work
updateTabsetPanel(session, "nav_radiant", selected = "Correlation") 

summary_correlation()
plots_correlation()

updateTabsetPanel(session, "nav_radiant", selected = "Code") 


# Loading previous state from a fixed location
# observe({
#   if(is.null(input$loadState) || input$loadState == 0) return()

#   # Joe Cheng: https://github.com/rstudio/shiny/issues/331
#   if(file.exists("state/RadiantValues.rds")) 
#     values <<- do.call(reactiveValues, readRDS("state/RadiantValues.rds"))

#   if(file.exists("state/RadiantInputs.rds")) 
#     state_list <<- readRDS("state/RadiantInputs.rds")
# })

# Saving current state from a fixed location
# observe({
#   if(is.null(input$saveState) || input$saveState == 0) return()
#   # save app state
#   saveRDS(isolate(reactiveValuesToList(input)), file = "state/RadiantInputs.rds")
#   saveRDS(isolate(reactiveValuesToList(values)), file = "state/RadiantValues.rds")
# })
