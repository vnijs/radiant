# run this separately on mac and windows
# install.packages('devtools')

# install.packages("radiant", repos = "http://cran.rstudio.com")
# install.packages("radiant", repos = "http://vnijs.github.io/radiant_miniCRAN/")

# next line not working - run from git terminal
#system("sh C:/Users/vnijs/Desktop/GitHub/radiant/build/git_win.sh")

library(devtools)
document(roclets = c('rd', 'collate', 'namespace'))
# build('../shinyAce', binary = TRUE)
# build('../rpivotTable', binary = TRUE)
# build('../jsonlite', binary = TRUE)
# build('../htmlwidgets', binary = TRUE)
# build('../shiny', binary = TRUE)
# devtools::install('../MathJaxR')
build('../MathJaxR', binary = TRUE)
# install('../DT_vnijs')
# build('../DT_vnijs', binary = TRUE)
# install('../DT')
build('../DT', binary = TRUE)
build('../radiant', binary = TRUE)

setwd('../')
rfile <- Sys.glob("*zip")
setwd('radiant')

# file.copy(paste0("../",rfile),"Z:/Desktop/GitHub")
# file.remove(paste0("../",rfile))
