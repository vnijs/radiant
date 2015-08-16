## ui for quant class - radiant
source("quant_ui.R")
# source(file.path(bp,"quant/quant_ui.R"), encoding = "UTF-8", local = TRUE)

shinyUI(
  do.call(navbarPage, c("Quantitative Analysis", nav_ui, quant_ui, shared_ui))
)
