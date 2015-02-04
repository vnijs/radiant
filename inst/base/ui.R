shinyUI(navbarPage("Radiant - Base", id = "nav_radiant", inverse = TRUE,
        collapsible = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Radiant help", uiOutput("help_base")),
    tabPanel("Radiant videos", uiOutput("help_videos")),
    # do NOT remove withMathJax(), load-files breaks!
    # tabPanel("About", withMathJax(), uiOutput("help_about"))
    tabPanel("About", uiOutput("help_about"))
  ),

  tags$head(tags$link(rel="shortcut icon", href="imgs/icon.png"))
))
