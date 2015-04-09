library(devtools)
document(roclets=c('rd', 'collate', 'namespace'))
build('../radiant_dev')
build('../radiant_dev', binary = TRUE)

build('../shinyAce')
build('../shinyAce', binary = TRUE)

build('../rpivotTable')
build('../rpivotTable', binary = TRUE)

build('../DT')
build('../DT', binary = TRUE)
