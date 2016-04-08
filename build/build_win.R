# run this separately on mac and windows
# install.packages("devtools")
library(devtools)
document(roclets = c('rd', 'collate', 'namespace'))

# install('../shiny')
# build('../shiny', binary = TRUE)
#
# install('../NeuralNetTools')
# build('../NeuralNetTools', binary = TRUE)
#
# install('../knitr')
# build('../knitr', binary = TRUE)

# install('../DT')
# build('../DT', binary = TRUE)
# install('../DiagrammeR')
# build('../DiagrammeR', binary = TRUE)

build('../radiant', binary = TRUE)

setwd('../')
rfile <- Sys.glob("*zip")
setwd('radiant')
