#######################################
# Shiny interface for data functions
#######################################
# data ui and tabs
output$data_ui_and_tabs <- renderUI({
  list(
    includeCSS("../base/www/style.css"),
    includeScript("../base/www/js/jquery-ui.custom.min.js"),
    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        wellPanel(uiOutput("uiDatasets")),
        conditionalPanel(condition = "input.datatabs == 'Manage'", uiOutput("ui_Manage")),
        conditionalPanel(condition = "input.datatabs == 'View'", uiOutput("ui_View")),
        conditionalPanel(condition = "input.datatabs == 'Visualize'", uiOutput("ui_Visualize")),
        conditionalPanel(condition = "input.datatabs == 'Explore'", uiOutput("ui_Explore")),
        conditionalPanel(condition = "input.datatabs == 'Merge'", uiOutput("ui_Merge")),
        conditionalPanel(condition = "input.datatabs == 'Transform'", uiOutput("ui_Transform"))
      ),
      mainPanel(id = "datatabs",
        uiOutput("tabs_data")
      )
    )
  )
})

# data tabs
output$tabs_data <- renderUI({
  tabsetPanel(id = "datatabs",
    tabPanel("Manage",
      htmlOutput("htmlDataExample"),
      HTML('<label>10 (max) rows shown. See View-tab for details.</label>'),
      conditionalPanel(condition = "input.man_add_descr == false", HTML(dataDescriptionOutput('html'))),
      conditionalPanel(condition = "input.man_add_descr == true",
        HTML("<label>Add data description:</label>"),
        tags$textarea(id="man_data_descr", rows="10", cols="12", dataDescriptionOutput('md'))
      )
    ),
    tabPanel("View", dataTableOutput("dataviewer")),
    tabPanel("Visualize", plotOutput("visualize", width = "100%", height = "100%")),
    tabPanel("Explore", verbatimTextOutput("expl_summary"), plotOutput("expl_plots", width = "100%", height = "100%")),
    tabPanel("Merge", htmlOutput("mergePossible"), htmlOutput("mergeData1"), htmlOutput("mergeData2")),
    tabPanel("Transform", htmlOutput("transform_data"), verbatimTextOutput("transform_summary"))
  )
})
