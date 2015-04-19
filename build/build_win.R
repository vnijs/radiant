# run this separately on mac and windows
install.packages('devtools')

# install.packages("radiant", repos = "http://cran.rstudio.com")
# install.packages("radiant", repos = "http://vnijs.github.io/radiant_miniCRAN/")

# next line not working - run from git terminal
#system("sh C:/Users/vnijs/Desktop/GitHub/radiant_dev/build/git_win.sh")

library(devtools)
document(roclets=c('rd', 'collate', 'namespace'))
build('../shinyAce', binary = TRUE)
build('../rpivotTable', binary = TRUE)
build('../jsonlite', binary = TRUE)
build('../htmlwidgets', binary = TRUE)
build('../shiny', binary = TRUE)
build('../radiant_dev', binary = TRUE)

setwd('../')
rfile <- Sys.glob("*zip")
setwd('radiant_dev')

# file.copy(paste0("../",rfile),"Z:/Desktop/GitHub")
# file.remove(paste0("../",rfile))
