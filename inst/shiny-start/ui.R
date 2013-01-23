shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Radyant - Shiny"),
  
  sidebarPanel(

  ),
  
  mainPanel(
    tabsetPanel(id = "analysistabs",
      tabPanel("Summary", verbatimTextOutput("summary"))
    )
  )
))