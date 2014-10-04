# Specify list of packages needed
pkgs_cran <- c("mime","BH","GPArotation","R.methodsS3", "R.oo", "R.utils", "car",
	"gridExtra", "psych", "vegan", "lubridate", "RColorBrewer",
	"wordcloud", "AlgDesign", "markdown", "rmarkdown", "knitr", "reshape2", "plyr", "ggplot2")

pkgs_gh <- c("shiny","shinyAce","rvest","tidyr","dplyr")

pkgs <- c(pkgs_cran,pkgs_gh)

# remove reshape2 and plyr when possible
