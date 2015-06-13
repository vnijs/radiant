# run this separately on mac and windows
library(devtools)
document(roclets = c('rd', 'collate', 'namespace'))
# build('../shinyAce', binary = TRUE)
# build('../rpivotTable', binary = TRUE)
# build('../MathJaxR', binary = TRUE)
# devtools::install('../DT')
build('../DT', binary = TRUE)
build('../radiant', binary = TRUE)

setwd('../')
rfile <- Sys.glob("*zip")
setwd('radiant')
