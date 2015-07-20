# devtools::install_github('hadley/devtools')
library(devtools)
document(roclets = c('rd', 'collate', 'namespace'))

build('../radiant')
build('../radiant', binary = TRUE)

# devtools::install('~/gh/DT')
# packageVersion("DT")
build('../DT')
build('../DT', binary = TRUE)

# install('../shinyAce')
# build('../shinyAce')
# build('../shinyAce', binary = TRUE)

# devtools::install('../rpivotTable')
# build('../rpivotTable')
# build('../rpivotTable', binary = TRUE)

# devtools::install('../MathJaxR')
# build('../MathJaxR')
# build('../MathJaxR', binary = TRUE)
