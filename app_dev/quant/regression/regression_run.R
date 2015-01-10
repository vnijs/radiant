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

  # dummy functions, when using alternate functions styling is messed up
	help_and_report <- function(...) {}
	inclMD <- function(...) {}

	source("regression.R", local = TRUE)
	source("regression_ui.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
