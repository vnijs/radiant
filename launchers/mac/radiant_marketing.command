#!/usr/bin/env Rscript
if(!require(radiant)) {
	options(repos = c(XRAN = "http://mostly-harmless.github.io/radiant_miniCRAN/"))
	install.packages('radiant')
}

require(radiant)
shiny::runApp(system.file('marketing', package='radiant'), port = 4475, launch.browser = TRUE)
