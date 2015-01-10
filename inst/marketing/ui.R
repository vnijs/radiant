shinyUI(
  navbarPage("Radiant - Marketing", id = "nav_radiant", inverse = TRUE,
             collapsable = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("Random",
    tabPanel("Central Limit Theorem", uiOutput("ctl")),
    tabPanel("Sampling", uiOutput("random")),
    tabPanel("Sample size", uiOutput("sampleSize"))
  ),

  navbarMenu("Base",
    # tabPanel("Single mean", uiOutput("singleMean")),
    tabPanel("Single mean", uiOutput("single_mean")),
    tabPanel("Compare means", uiOutput("compareMeans")),
    tabPanel("Cross-tabs", uiOutput("crosstab"))
  ),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear (OLS)", uiOutput("regression"))
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
    tabPanel("Create profiles", uiOutput("conjointProfiles")),
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
    tabPanel("About", withMathJax(), uiOutput("help_about"))
  )
))
