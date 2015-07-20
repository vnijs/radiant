shinyUI(
  do.call(navbarPage, c("Quantitative Analysis", nav_ui, quant_ui, shared_ui))
)
