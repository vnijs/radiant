# avoid breaks in R-output print and show JSON packets transferred
# over websockets
options(width = 150, shiny.trace=TRUE)
# options(width = 150)

options(repos = c("http://cran.rstudio.com/"))
libs <- c("shiny", "car", "AER", "Ecdat", "foreign", "tools", "ggplot2", 
	"gridExtra", "reshape2", "plyr", "markdown", "R.utils", "psych", "rela", "arm", "xts")
available <- suppressWarnings(suppressPackageStartupMessages(sapply(libs, require, character.only=TRUE)))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
	install.packages(inst.libs, dependencies = TRUE)
	suppressWarnings(suppressPackageStartupMessages(sapply(inst.libs, require, character.only=TRUE)))
}

actionButton <- function(inputId, label) {
  tagList(
    singleton(tags$head(tags$script(src = 'js/actionbutton.js'))),
    tags$button(id=inputId, type="button", class="btn action-button", label)
  )
}

# setting up a few standard datasets to play with 
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

# Our datasets can change over time (i.e. the changedata function). Therefore,
# these need to be reactive values; otherwise, the other reactive functions
# and outputs that depend on these datasets won't know that they are changed.

# Note that we never get or assign the "original" copies of mtcars, morley, 
# or rock. This way, all user sessions are independent from each other 

values <- reactiveValues()
values$mtcars <- mtcars
values$morley <- morley
values$rock <- rock

n <- nrow(diamonds)

values$diamonds <- diamonds[sample(1:n,3000),]

datasets <- c("mtcars", "diamonds", "rock")

# loading list of data.frame in the car package
# listPackData <- function(packs) {
# 	libnames <- c('')
# 	lib <- c('')
# 	for(ipack in packs) {
# 		ilib <- data(package = ipack)$results
# 		libnames <- c(libnames, paste(ilib[,'Package'],ilib[,'Item'], sep = '-'))
# 		lib <- c(lib,ilib[,'Item'])
# 	}
# 	names(lib) <- libnames
# 	as.list(lib)
# }

# packDataSets <- listPackData(c('AER','Ecdat'))

# pds <- list()
# for(pd in 2:length(packDataSets)) {
# 	data(list = packDataSets[[pd]])
# 	dat <- get(packDataSets[[pd]])
# 	if(!is.null(colnames(dat))) {
# 		pds <- c(pds,packDataSets[pd])
# 	}
# }
# # packDataSets <- packDataSets[-which(packDataSets == "DutchSales")]
# # packDataSets <- c(list(''=''),pds)
# packDataSets <- c('',pds)
# save(packDataSets, file = '~/Desktop/packDataSets.rda')

load('data/packDataSets.rda')

lastLoaded <- "" 		

includeRmd <- function(path){
  if (!require(knitr))
    stop("knitr package is not installed")
  if (!require(markdown))
    stop("Markdown package is not installed")
  shiny:::dependsOnFile(path)
  html <- knitr::knit2html(path, fragment.only = TRUE)
  includeHTML(html)
  # Encoding(html) <- 'UTF-8'
  # HTML(html)
}

# Simulate a big data-file
# n <- 200000
# n.var <- 100
# bigsimdat <- data.frame(matrix(rnorm(n*n.var),nrow = n, ncol <- n.var))
# save(simdat,file = "data/bigsimdat.rda")