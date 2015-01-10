rm(list = ls())

options(repos = c(CRAN = "http://cran.rstudio.com"))

if(!require(broom))
	install.packages("broom"); library(broom)

library(ggplot2)
library(shiny)
library(magrittr)
library(dplyr)

setwd("~/Desktop/GitHub/radiant_dev/app_dev/quant/regression")

ui <- bootstrapPage(

  selectInput(inputId = "dataset", label = "",
              choices = c("mtcars"),
    					selected = "mtcars",
    					multiple = FALSE),
  uiOutput("regression")
)

server <- function(input, output, session) {

	state_list <- list()
 	values <- reactiveValues()
  values[["mtcars"]] <- mtcars
  values[["mtcars_descr"]] <- ""
  values$datasetlist <- c("mtcars")
  values$plotWidth <- 600
  values$plotHeight <- 600

	source('../../../R/radiant.R', local = TRUE)

	# dummy functions, help not tested (yet)
  ##
  ## Could this be a function that creates a popup of the text that
  ## would be sent to the report function??
  ##
	help_and_report <- function(...) {}

  ##
  ## Could this be the same as inclMD in radiant.R but that it 'fixes'
  ## the url to the help file? i.e., adds ../ as in paste0("../",path)
  ##
	inclMD <- function(...) {}

	source("regression.R", local = TRUE)
	source("regression_ui.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
