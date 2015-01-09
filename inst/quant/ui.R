shinyUI(navbarPage("Radiant - Quantitative Analysis", id = "nav_radiant", inverse = TRUE, collapsable = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("Random",
    tabPanel("Central Limit Theorem", uiOutput("ctl")),
    tabPanel("Sampling", uiOutput("random")),
    tabPanel("Sample size", uiOutput("sampleSize"))
  ),

  navbarMenu("Base",
    # update when all elements of base have been upgraded and tested
    tabPanel("Single mean", uiOutput("singleMean")),
    # tabPanel("Single mean", uiOutput("single_mean")),
    tabPanel("Compare means", uiOutput("compareMeans")),
    tabPanel("Single proportion", uiOutput("singleProp")),
    tabPanel("Cross-tabs", uiOutput("crosstab"))
  ),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear (OLS)", uiOutput("regression"))
  ),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Radiant help", uiOutput("help_quant")),
    tabPanel("Radiant videos", uiOutput("help_videos")),
    # do NOT remove withMathJax(), load-files breaks!
    tabPanel("About", withMathJax(), uiOutput("help_about"))
  )
))
