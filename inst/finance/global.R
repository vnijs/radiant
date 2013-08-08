nasdaq_file <- read.csv('data/nasdaq-company-list.csv', stringsAsFactors = FALSE)
symbol_list <- c("",nasdaq_file[,'Symbol'])
names(symbol_list) <- c("",nasdaq_file[,'Name'])
symbol_list <- as.list(symbol_list)

lastLoadedYahooData = ""

actionButton <- function(inputId, label) {
  tagList(
    singleton(tags$head(tags$script(src = 'js/actionbutton.js'))),
    tags$button(id=inputId, type="button", class="btn action-button", label)
  )
}

options(repos = c("http://cran.rstudio.com/"))

libs <- c("shiny", "car", "foreign", "tools", "ggplot2", 
	"gridExtra", "markdown", "R.utils", "fGarch", "quantmod")
  # "gridExtra", "reshape2", "plyr", "markdown", "R.utils", "psych", "rela", "arm", "xts")
available <- suppressWarnings(suppressPackageStartupMessages(sapply(libs, require, character.only=TRUE)))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
	install.packages(inst.libs, dependencies = TRUE)
	suppressWarnings(suppressPackageStartupMessages(sapply(inst.libs, require, character.only=TRUE)))
}