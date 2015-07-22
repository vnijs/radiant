## ui for base radiant
shinyUI(
  do.call(navbarPage, c("Radiant", nav_ui, shared_ui))
)

