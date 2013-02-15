library(shiny)
library(shinyIncubator)
library(car)
library(tools)
library(foreign)
library(ggplot2)
library(gridExtra)
library(fGarch)
library(quantmod)
library(R.utils)

# avoid breaks in R-output print and show JSON packets transferred
# over websockets
options(width = 150, shiny.trace=TRUE)

shinyServer(function(input, output) {

	values <- reactiveValues()

	robjname <- load('data/GSPC.rda')
	values[[robjname]] <- get(robjname)
	datasets <- c(robjname)

	# source base functions
	source('radyant.R', local = TRUE)

	# source enabled analysis tools
	sourceDirectory('tools_enabled/')

	# the 'grand' ui-element caller
	output$analysis_ui_controls <- renderUI({
  	if(input$tool == "dataview") return()
	  get(paste('ui_',input$tool, sep=""))()
	})
})