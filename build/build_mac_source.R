# devtools::install_github('hadley/devtools')
library(devtools)
document(roclets = c('rd', 'collate', 'namespace'))

build('../radiant')
build('../radiant', binary = TRUE)

# devtools::install('~/gh/DT')
# packageVersion("DT")
# build('../DT')
# build('../DT', binary = TRUE)

# install('../shiny')
# build('../shiny')
# build('../shiny', binary = TRUE)
#
# install('../NeuralNetTools')
# build('../NeuralNetTools')
# build('../NeuralNetTools', binary = TRUE)
#
# install('../knitr')
# build('../knitr')
# build('../knitr', binary = TRUE)

# install('../DiagrammeR')
# build('../DiagrammeR')
# build('../DiagrammeR', binary = TRUE)




