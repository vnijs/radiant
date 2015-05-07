# devtools::install_github('hadley/devtools')
library(devtools)
document(roclets=c('rd', 'collate', 'namespace'))
build('../radiant_dev')
build('../radiant_dev', binary = TRUE)

# install('../shinyAce')
build('../shinyAce')
build('../shinyAce', binary = TRUE)

# install('../rpivotTable')
build('../rpivotTable')
build('../rpivotTable', binary = TRUE)

# install('../jsonlite')
build('../jsonlite')
build('../jsonlite', binary = TRUE)

# install('../shiny')
build('../shiny')
build('../shiny', binary = TRUE)

# install('../htmlwidgets')
build('../htmlwidgets')
build('../htmlwidgets', binary = TRUE)

# install('../DT')
build('../DT')
build('../DT', binary = TRUE)
