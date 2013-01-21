library(shiny)
library(shinyIncubator)
library(car)
library(datasets)
library(tools)
library(foreign)
library(ggplot2)
library(gridExtra)
library(fGarch)
library(quantmod)

options(width = 150, shiny.trace=TRUE)
# options(width = 150)

lastLoadedYahooData = ""