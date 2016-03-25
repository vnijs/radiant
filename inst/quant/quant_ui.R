quant_ui <- tagList(
  navbarMenu("Sample",
    tabPanel("Sampling", uiOutput("sampling")),
    # tabPanel("Sample size", uiOutput("sample_size"))
    tabPanel("Sample size (single)", uiOutput("sample_size")),
    tabPanel("Sample size (compare)", uiOutput("sample_size_comp"))
  ),

  navbarMenu("Base",
    tabPanel("Probability calculator", uiOutput("prob_calc")),
    tabPanel("Central Limit Theorem", uiOutput("clt")),
    tabPanel("Single mean", uiOutput("single_mean")),
    tabPanel("Compare means", uiOutput("compare_means")),
    tabPanel("Single proportion", uiOutput("single_prop")),
    tabPanel("Compare proportions", uiOutput("compare_props")),
    tabPanel("Cross-tabs", uiOutput("cross_tabs"))
  ),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear regression (OLS)", uiOutput("regression")),
    tabPanel("Logistic regression (GLM)", uiOutput("glm_reg"))
  ),

  navbarMenu("Decide",
    tabPanel("Decision tree", uiOutput("dtree")),
    tabPanel("Simulate", uiOutput("simulater"))
  )
)
