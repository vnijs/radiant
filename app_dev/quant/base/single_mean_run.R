###############################################
# turn single_mean into a mini-package
# to see if you can (1) compile rmd outside
# of radiant, (2) report within radiant, and
# (3) see if you can embed the package ui in a
# dynamic document

# From previous setup ...
# install.packages('radiant',repos=c("http://vnijs.rady.ucsd.edu/site_media/R", CRAN = "http://cran.rstudio.com"), dependencies = TRUE)
# shiny::runApp(system.file('marketing', package='radiant'), port = 8100)
###############################################

rm(list = ls())

options(repos = c(CRAN = "http://cran.rstudio.com"))

if(!require(broom))
	install.packages("broom"); library(broom)

library(ggplot2)
library(shiny)
library(magrittr)
library(dplyr)

setwd("~/Desktop/GitHub/radiant_dev/app_dev/quant/base")

ui <- bootstrapPage(

  selectInput(inputId = "dataset", label = "",
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

	source('../../../R/radiant.R', local = TRUE)

	# dummy functions, when using alternate functions styling is messed up
	help_and_report <- function(...) {}
	inclMD <- function(...) {}
	# inclMD <- function(path) {
	# 	path <- paste0("../../../inst/base/",path)
 	# 	markdown::markdownToHTML(path, options = c(""),
 	#                            stylesheet="../../../inst/base/www/empty.css")
  #	}

	source("single_mean.R", local = TRUE)
	source("single_mean_ui.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
