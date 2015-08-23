help_menu <-
  tagList(
    navbarMenu(title = "", id = "Help", icon = icon("question-circle"),
      tabPanel("Help", uiOutput("help_quant"), icon = icon("question")),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a("", href = "http://vnijs.github.io/radiant/", target = "_blank",
               list(icon("globe"), "Radiant docs"))),
      tabPanel(tags$a("", href = "https://github.com/vnijs/radiant/issues", target = "_blank",
               list(icon("github"), "Report issue")))

    ),
    js_head
  )

## ui for quant class - radiant
source("quant_ui.R")
# source(file.path(bp,"quant/quant_ui.R"), encoding = "UTF-8", local = TRUE)

shinyUI(
  do.call(navbarPage, c("Quantitative Analysis", nav_ui, quant_ui,
                        shared_ui, help_menu))
)
