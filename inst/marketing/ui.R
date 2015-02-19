shinyUI(
  navbarPage("Radiant - Marketing", id = "nav_radiant", inverse = TRUE,
             collapsible = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("Random",
    tabPanel("Central Limit Theorem", uiOutput("ctl")),
    tabPanel("Sampling", uiOutput("random")),
    tabPanel("Sample size", uiOutput("sampleSize"))
  ),

  navbarMenu("Base",
    tabPanel("Single mean", uiOutput("single_mean")),
    tabPanel("Compare means", uiOutput("compare_means")),
    tabPanel("Single proportion", uiOutput("single_prop")),
    tabPanel("Compare proportions", uiOutput("compare_props")),
    tabPanel("Cross-tabs", uiOutput("crosstab"))
  ),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear (OLS)", uiOutput("regression")),
    tabPanel("GLM", uiOutput("glm_reg"))
  ),

  navbarMenu("Maps",
    tabPanel("(Dis)similarity", uiOutput("mds")),
    tabPanel("Attributes", uiOutput("pmap"))
  ),

  navbarMenu("Factor",
    tabPanel("Pre-factor", uiOutput("preFactor")),
    tabPanel("Factor", uiOutput("fullFactor"))
  ),

  navbarMenu("Cluster",
    tabPanel("Hierarchical", uiOutput("hierCluster")),
    tabPanel("K-means", uiOutput("kmeansCluster"))
  ),

  navbarMenu("Conjoint",
    tabPanel("Create profiles", uiOutput("conjoint_profiles")),
    tabPanel("Conjoint", uiOutput("conjoint"))
  ),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Radiant help", uiOutput("help_marketing")),
    tabPanel("Radiant videos", uiOutput("help_videos")),
    # do NOT remove withMathJax(), load-files breaks!
    # tabPanel("About", withMathJax(), uiOutput("help_about"))
    tabPanel("About", uiOutput("help_about"))
  ),

  tags$head(tags$link(rel="shortcut icon", href="imgs/icon.png"))
))
