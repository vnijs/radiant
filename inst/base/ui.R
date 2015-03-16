shinyUI(navbarPage("Radiant - Base", id = "nav_radiant", inverse = TRUE,
        collapsible = TRUE,

  tabPanel("Data", withMathJax(), uiOutput('ui_data')),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Radiant help", uiOutput("help_base")),
    tabPanel("Radiant videos", uiOutput("help_videos")),
    tabPanel("About", uiOutput("help_about"))
  ),

  tags$head(tags$link(rel="shortcut icon", href="imgs/icon.png"))
))
