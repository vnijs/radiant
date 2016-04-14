library(shinyapps)
devtools::install_github("rich-iannone/DiagrammeR")
# install.packages("DiagrammeR")
# devtools::install_github("fawda123/NeuralNetTools")
devtools::install_github("vnijs/radiant")

## update as needed
fpath <- "~/gh/radiant/inst/base"
account <- "vnijs"
app <- "analytics"
setwd(fpath)

for (file in list.files("../../../shinyapps/R", pattern = "\\.(r|R)$", full.names = TRUE))
  source(file, local = TRUE)

source("../../build/deployapp.R", local = TRUE)

setwd(file.path(fpath,"../",app))
deployApp(account = account, launch.browser = FALSE)

