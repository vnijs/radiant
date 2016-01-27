help_menu <-
  tagList(
    navbarMenu(title = "", id = "Help", icon = icon("question-circle"),
      tabPanel("Help", uiOutput("help_analytics"), icon = icon("question")),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a("", href = "http://vnijs.github.io/radiant/", target = "_blank",
               list(icon("globe"), "Radiant docs"))),
      tabPanel(tags$a("", href = "https://github.com/vnijs/radiant/issues", target = "_blank",
               list(icon("github"), "Report issue")))
    ),
    js_head
  )

# source(file.path(r_path, "quant/quant_ui.R"), encoding = "UTF-8", local = TRUE)
  # library(radiant)
  # library(shiny)
  # r_encoding <- "UTF-8"
  # r_path <- "~/gh/radiant/inst"

# source(file.path(r_path, "quant/quant_ui.R"), encoding = r_encoding, local = TRUE)
analytics_ui <- tagList(
  navbarMenu("Sample",
    tabPanel("Sampling", uiOutput("sampling")),
    tabPanel("Sample size", uiOutput("sample_size"))
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
    tabPanel("Linear (OLS)", uiOutput("regression")),
    tabPanel("GLM", uiOutput("glm_reg"))
  ),

  # navbarMenu("Model",
  #   tabPanel("Neural Network (ANN)", uiOutput("ann")),
  #   tabPanel("Model performance", uiOutput("performance"))
  # ),

  navbarMenu("Decide",
    tabPanel("Decision tree", uiOutput("dtree")),
    tabPanel("Simulate", uiOutput("simulater"))
  ),

  navbarMenu("Cluster",
    tabPanel("Hierarchical", uiOutput("hier_clus")),
    tabPanel("K-means", uiOutput("kmeans_clus"))
  )
)

shinyUI(
  do.call(navbarPage, c("Customer Analytics", nav_ui, analytics_ui,
                        shared_ui, help_menu))
)
