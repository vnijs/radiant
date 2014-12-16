shinyUI(
  navbarPage("Radiant - Base", id = "nav_radiant", inverse = TRUE,
             collapsable = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Radiant help", uiOutput("help_base")),
    tabPanel("Radiant videos", withMathJax(),
             "../base/tools/app/videos_quant.Rmd" %>%
               inclRmd %>% HTML),
    # do NOT remove withMathJax(), load-files breaks!
    tabPanel("About", withMathJax(), "../base/tools/app/about.Rmd" %>%
             inclRmd %>% HTML)
  )
))
