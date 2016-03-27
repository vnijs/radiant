help_menu <-
  tagList(
    # navbarMenu(title = "", id = "Help", icon = icon("question-circle"),
    navbarMenu(title = "", icon = icon("question-circle"),
      tabPanel("Help", uiOutput("help_marketing"), icon = icon("question")),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a("", href = "http://vnijs.github.io/radiant/", target = "_blank",
               list(icon("globe"), "Radiant docs"))),
      tabPanel(tags$a("", href = "https://github.com/vnijs/radiant/issues", target = "_blank",
               list(icon("github"), "Report issue")))
    ),
    js_head
  )


# source(file.path(r_path,"quant/quant_ui.R"), encoding = "UTF-8", local = TRUE)
source(file.path(r_path,"quant/quant_ui.R"), encoding = r_encoding, local = TRUE)

marketing_ui <- tagList(
  navbarMenu("Maps",
    tabPanel("(Dis)similarity", uiOutput("mds")),
    tabPanel("Attributes", uiOutput("pmap"))
  ),

  navbarMenu("Factor",
    tabPanel("Pre-factor", uiOutput("pre_factor")),
    tabPanel("Factor", uiOutput("full_factor"))
  ),

  navbarMenu("Cluster",
    tabPanel("Hierarchical", uiOutput("hier_clus")),
    tabPanel("K-means", uiOutput("kmeans_clus"))
  ),

  navbarMenu("Conjoint",
    tabPanel("Create profiles", uiOutput("doe")),
    tabPanel("Conjoint", uiOutput("conjoint"))
  )
)

shinyUI(
  do.call(navbarPage, c("Marketing Research", nav_ui, quant_ui, marketing_ui,
                        shared_ui, help_menu))
)
