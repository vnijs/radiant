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

source(file.path(r_path, "quant/quant_ui.R"), encoding = "UTF-8", local = TRUE)

analytics_ui <- tagList(
  navbarMenu("Cluster",
    tabPanel("Hierarchical", uiOutput("hier_clus")),
    tabPanel("K-means", uiOutput("kmeans_clus"))
  )
)

shinyUI(
  do.call(navbarPage, c("Customer Analytics", nav_ui, quant_ui, analytics_ui,
                        shared_ui, help_menu))
)
