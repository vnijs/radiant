# run this separately on mac and windows
library(devtools)
document(roclets = c('rd', 'collate', 'namespace'))
# build('../shinyAce', binary = TRUE)
# build('../MathJaxR', binary = TRUE)
# devtools::install('../DT')
# build('../DT', binary = TRUE)
# devtools::install('../data.tree')
build('../data.tree', binary = TRUE)
build('../radiant', binary = TRUE)

setwd('../')
rfile <- Sys.glob("*zip")
setwd('radiant')
