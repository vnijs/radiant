# run this separately on mac and windows
# install.packages('devtools')
library(devtools)
document(roclets=c('rd', 'collate', 'namespace'))
build('../radiant_dev', binary = TRUE)

setwd('../')
rfile <- Sys.glob("radiant*zip")
setwd('radiant_dev')

file.copy(paste0("../",rfile),"Z:/Desktop/GitHub")
file.remove(paste0("../",rfile))
