#!/usr/bin/env Rscript
if(!require(radiant)) {
	options(repos = c(XRAN = "http://mostly-harmless.github.io/radiant_miniCRAN/"))
	install.packages('radiant')
}

shiny::runApp(system.file('quant', package='radiant'), port = 4403, launch.browser = TRUE)
