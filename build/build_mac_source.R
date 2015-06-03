# devtools::install_github('hadley/devtools')
library(devtools)
document(roclets=c('rd', 'collate', 'namespace'))
build('../radiant')
build('../radiant', binary = TRUE)

# install('../shinyAce')
# build('../shinyAce')
# build('../shinyAce', binary = TRUE)

# devtools::install('../rpivotTable')
# build('../rpivotTable')
# build('../rpivotTable', binary = TRUE)

# devtools::install('~/gh/DT')
build('../DT')
build('../DT', binary = TRUE)

# devtools::install('../DT_vnijs')
# build('../DT_vnijs')
# build('../DT_vnijs', binary = TRUE)

# devtools::install('../MathJaxR')
build('../MathJaxR')
build('../MathJaxR', binary = TRUE)

