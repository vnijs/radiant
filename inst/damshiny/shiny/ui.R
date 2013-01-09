shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Data Analysis Menu in Shiny"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(

  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(id = "analysistabs",
      tabPanel("Summary", verbatimTextOutput("summary"))
    )
  )
))