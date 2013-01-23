library(shiny)
library(shinyIncubator)
library(car)
library(tools)
library(foreign)
library(ggplot2)
library(gridExtra)
library(R.utils)

# avoid breaks in R-output print, don't show error messages in Rstudio
options(width = 150, shiny.trace=TRUE)


shinyServer(function(input, output) {

	# making a local copy of a few commonly used dataset
	mtcars <- mtcars
	mtcars$vs <- as.factor(mtcars$vs)
	mtcars$am <- as.factor(mtcars$am)
	morley <- morley
	rock <- rock

	# Our datasets can change over time (i.e. the changedata function). Therefore,
	# these need to be reactive values; otherwise, the other reactive functions
	# and outputs that depend on these datasets won't know that they are changed.

	# Note that we never get or assign the "original" copies of mtcars, morley, 
	# or rock. This way, all user sessions are independent from each other 

	values <- reactiveValues()
	values$mtcars <- mtcars
	values$morley <- morley
	values$rock <- rock
	# values$diamonds <- diamonds[1:1000,]
	# datasets <- c("diamonds","mtcars", "morley", "rock")
	datasets <- c("mtcars", "morley", "rock")

	# source base functions
	source('radyant.R', local = TRUE)

	# source enabled analysis tools
	sourceDirectory('tools_enabled/')

	# the 'grand' ui-element caller
	output$analysis_ui_controls <- reactiveUI(function() {
  	if(input$tool == "dataview") return()
	  get(paste('ui_',input$tool, sep=""))()
	})

})