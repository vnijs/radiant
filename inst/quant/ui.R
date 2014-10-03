shinyUI(navbarPage("Radiant - Quantitative Analysis", id = "nav_radiant", inverse = TRUE, collapsable = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("Random",
    tabPanel("Sampling and assignment", uiOutput("random")),
    tabPanel("Sample size", uiOutput("sampleSize"))
  ),

  navbarMenu("Base",
    tabPanel("Single mean", uiOutput("singleMean")),
    tabPanel("Compare means", uiOutput("compareMeans"))
  ),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear (OLS)", uiOutput("regression"))
  ),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("State", uiOutput("state")),
  tabPanel("About", withMathJax(), HTML(inclRmd("../base/tools/app/about.Rmd"))),
  tabPanel("Quit", uiOutput("quit"))

))
