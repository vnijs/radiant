shinyUI(navbarPage("Base", windowTitle = "Radiant", id = "nav_radiant",
        inverse = TRUE, collapsible = TRUE,

  tabPanel("Data", withMathJax(), uiOutput('ui_data')),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Help", uiOutput("help_base")),
    tabPanel("Videos", uiOutput("help_videos")),
    tabPanel("About", uiOutput("help_about"))
  ),

  tags$head(tags$script(src="js/session.js"),
            tags$link(rel="shortcut icon", href="imgs/icon.png"))
))
