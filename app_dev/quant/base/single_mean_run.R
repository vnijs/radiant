
								###############################
								###############################
								###############################
								# turn single_mean into a mini-package
								# to see if you can (1) compile rmd outside
								# of radiant, (2) report within radiant, and
								# (3) see if you can embed the package ui in a
								# dynamic document
								###############################
								###############################
								###############################

rm(list = ls())

options(repos = c(CRAN = "http://cran.rstudio.com"))

if(!require(broom))
	install.packages("broom"); library(broom)

library(ggplot2)
library(shiny)
library(magrittr)
library(dplyr)

setwd("~/Desktop/GitHub/radiant_dev/app_dev/quant")

ui <- bootstrapPage(

  selectInput(inputId = "datasets", label = "",
              choices = c("mtcars"),
    					selected = "mtcars",
    					multiple = FALSE),
  uiOutput("single_mean")
)

server <- function(input, output, session) {

	state_list <- list()
 	values <- reactiveValues()
  values[["mtcars"]] <- mtcars
  values[["mtcars_descr"]] <- ""
  values$datasetlist <- c("mtcars")
  values$plotWidth <- 600
  values$plotHeight <- 600

	source('../../R/radiant.R', local = TRUE)

	# dummy functions, help not tested (yet)
	help_and_report <- function(...) {}
	inclMD <- function(...) {}

	source("base.R", local = TRUE)
	source("base_ui.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
