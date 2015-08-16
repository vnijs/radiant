source(file.path(r_path, "quant/quant_ui.R"), encoding = "UTF-8", local = TRUE)

analytics_ui <- tagList(
  navbarMenu("Cluster",
    tabPanel("Hierarchical", uiOutput("hier_clus")),
    tabPanel("K-means", uiOutput("kmeans_clus"))
  ),

  navbarMenu("Conjoint",
    tabPanel("Create profiles", uiOutput("conjoint_profiles")),
    tabPanel("Conjoint", uiOutput("conjoint"))
  )
)

shinyUI(
  do.call(navbarPage, c("Analyics", nav_ui, quant_ui, analytics_ui, shared_ui))
)
