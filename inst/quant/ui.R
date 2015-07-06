shinyUI(
  navbarPage("Quantitative Analysis", windowTitle = "Radiant",
             id = "nav_radiant", inverse = TRUE, collapsible = TRUE,

  tabPanel("Data", withMathJax(), uiOutput('ui_data')),

  navbarMenu("Sample",
    tabPanel("Sampling", uiOutput("sampling")),
    tabPanel("Sample size", uiOutput("sample_size"))
  ),

  navbarMenu("Base",
    tabPanel("Single mean", uiOutput("single_mean")),
    tabPanel("Compare means", uiOutput("compare_means")),
    tabPanel("Single proportion", uiOutput("single_prop")),
    tabPanel("Compare proportions", uiOutput("compare_props")),
    tabPanel("Cross-tabs", uiOutput("cross_tabs"))
  ),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear (OLS)", uiOutput("regression")),
    tabPanel("GLM", uiOutput("glm_reg"))
  ),

  navbarMenu("Decide",
    tabPanel("Simulate", uiOutput("simulater"))
  ),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Help", uiOutput("help_quant")),
    tabPanel("Videos", uiOutput("help_videos")),
    tabPanel("About", uiOutput("help_about"))
  ),

  tags$head(tags$script(src="js/session.js"),
            tags$link(rel="shortcut icon", href="imgs/icon.png"))
))
