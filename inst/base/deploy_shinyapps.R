# deplory shinyapps.io
devtools::install_github("vnijs/radiant")
library(shinyapps)
fpath <- "~/gh/radiant_dev/inst/base"
setwd(fpath)
deploy()
setwd(file.path(fpath,"../quant"))
deploy()
setwd(file.path(fpath,"../marketing"))
deploy()
