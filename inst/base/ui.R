help_menu <-
  tagList(
    navbarMenu("", icon = icon("question-circle"),
      tabPanel("Help", uiOutput("help_base"), icon = icon("question")),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a("", href = "http://vnijs.github.io/radiant/", target = "_blank",
               list(icon("globe"), "Radiant docs"))),
      tabPanel(tags$a("", href = "https://github.com/vnijs/radiant/issues", target = "_blank",
               list(icon("github"), "Report issue")))
    ),
    js_head
  )

## ui for base radiant
shinyUI(
  do.call(navbarPage, c("Radiant", nav_ui, shared_ui, help_menu))
)
