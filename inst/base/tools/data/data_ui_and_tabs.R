#######################################
# Shiny interface for data functions
#######################################

# data ui and tabs
output$data_ui_and_tabs <- renderUI({
  list(
    includeCSS("../base/www/style.css"),
    includeScript("../base/www/js/jquery-ui.custom.min.js"),
    # includeScript("../base/www/js/select_add_class.js"),
    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        uiOutput("uiDatasets"),
        conditionalPanel("input.datatabs == 'Manage'",
                         uiOutput("ui_Manage")),
        conditionalPanel("input.datatabs == 'View'",
                         uiOutput("ui_View")),
        conditionalPanel("input.datatabs == 'Visualize'",
                         uiOutput("ui_Visualize")),
        conditionalPanel("input.datatabs == 'Explore'",
                         uiOutput("ui_Explore")),
        conditionalPanel("input.datatabs == 'Merge'",
                         uiOutput("ui_Merge")),
        conditionalPanel("input.datatabs == 'Transform'",
                         uiOutput("ui_Transform"))),
      mainPanel(id = "datatabs",
        uiOutput("tabs_data")
      )
    )
  )
})

# data tabs
output$tabs_data <- renderUI({
  tabsetPanel(id = "datatabs",
    tabPanel("Manage", htmlOutput("htmlDataExample"),
      conditionalPanel("input.man_add_descr == false",
                       uiOutput("dataDescriptionHTML")),
      conditionalPanel("input.man_add_descr == true",
                       uiOutput("dataDescriptionMD"))),
    tabPanel("View", dataTableOutput("dataviewer")),
    tabPanel("Visualize",
             plotOutput("visualize", width = "100%", height = "100%")),
    tabPanel("Explore", verbatimTextOutput("expl_summary"),
             plotOutput("expl_plots", width = "100%", height = "100%")),
    tabPanel("Merge", htmlOutput("mergePossible"),
             htmlOutput("mergeData1"), htmlOutput("mergeData2")),
    tabPanel("Transform", htmlOutput("transform_data"),
             verbatimTextOutput("transform_summary"))
  )
})
