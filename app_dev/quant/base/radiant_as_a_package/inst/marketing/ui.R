shinyUI(navbarPage("Radiant", id = "nav_radiant", inverse = TRUE, collapsable = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("Random",
    tabPanel("Sampling and assignment", uiOutput("random")),
    tabPanel("Sample size", uiOutput("sampleSize"))
  ),

  navbarMenu("Base",
    tabPanel("Single mean", uiOutput("singleMean")),
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
    tabPanel("Pre-factor analysis", uiOutput("preFactor")),
    tabPanel("Factor analysis", uiOutput("fullFactor"))
  ),

  navbarMenu("Cluster",
    tabPanel("Hierarchical", uiOutput("hierCluster")),
    tabPanel("K-means", uiOutput("kmeansCluster"))
  ),

  navbarMenu("Conjoint",
    tabPanel("Create profiles", uiOutput("conjointProfiles")),
    tabPanel("Conjoint analysis", uiOutput("conjoint"))
  ),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("State", uiOutput("state")),

  # tabPanel("About", includeRmd("../base/tools/app/about.Rmd"))
  tabPanel("About", includeRmd("tools/app/about.Rmd"))
))
