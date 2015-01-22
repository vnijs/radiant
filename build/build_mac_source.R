library(devtools)
document(roclets=c('rd', 'collate', 'namespace'))
build('../radiant_dev')
build('../radiant_dev', binary = TRUE)
