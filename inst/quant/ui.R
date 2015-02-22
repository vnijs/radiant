shinyUI(navbarPage("Radiant - Quantitative Analysis", id = "nav_radiant",
        inverse = TRUE, collapsible = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("Random",
    tabPanel("Central Limit Theorem", uiOutput("ctl")),
    tabPanel("Sampling", uiOutput("random")),
    tabPanel("Sample size", uiOutput("sampleSize"))
  ),

  navbarMenu("Base",
    # update when all elements of base have been upgraded and tested
    tabPanel("Single mean", uiOutput("single_mean")),
    tabPanel("Compare means", uiOutput("compare_means")),
    tabPanel("Single proportion", uiOutput("single_prop")),
    tabPanel("Compare proportions", uiOutput("compare_props")),
    tabPanel("Cross-tabs", uiOutput("cross_tabs"))
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
    # tabPanel("About", withMathJax(), uiOutput("help_about"))
    tabPanel("About", uiOutput("help_about"))
  ),

  tags$head(tags$link(rel="shortcut icon", href="imgs/icon.png"))
))
