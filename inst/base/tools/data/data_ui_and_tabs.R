#######################################
# Shiny interface for data functions
#######################################

output$uiView_err <- renderUI({
  if(r_data$error %>% is_empty) return()
  helpText(r_data$error)
})

# data ui and tabs
output$data_ui_and_tabs <- renderUI({
  list(
    includeCSS("../base/www/style.css"),
    includeScript("../base/www/js/jquery-ui.custom.min.js"),
    includeScript("../base/www/js/returnTextInputBinding.js"),
    includeScript("../base/www/js/returnTextAreaBinding.js"),
    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        wellPanel(
          uiOutput("uiDatasets"),
          conditionalPanel("input.datatabs != 'Manage'",
            checkboxInput('show_filter', 'Filter (e.g., price > 5000)',
                          value = state_init("show_filter",FALSE)),
            conditionalPanel("input.show_filter == true",
              returnTextAreaInput("data_filter", label = "",
                                  value = state_init("data_filter")),
              uiOutput("uiView_err")))
        ),
        conditionalPanel("input.datatabs == 'Manage'",
                         uiOutput("ui_Manage")),
        conditionalPanel("input.datatabs == 'View'",
                         uiOutput("ui_View")),
        conditionalPanel("input.datatabs == 'Visualize'",
                         uiOutput("ui_Visualize")),
        conditionalPanel("input.datatabs == 'Explore'",
                         uiOutput("ui_Explore")),
        conditionalPanel("input.datatabs == 'Pivot'",
                         uiOutput("ui_Pivot")),
        conditionalPanel("input.datatabs == 'Merge'",
                         uiOutput("ui_Merge")),
        conditionalPanel("input.datatabs == 'Transform'",
                         uiOutput("ui_Transform"))),
        #   conditionalPanel("input.datatabs != 'Manage'",
      mainPanel(
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
    tabPanel("Pivot", rpivotTableOutput("pivotData")),
    tabPanel("Merge", htmlOutput("mergePossible"),
             htmlOutput("mergeData1"), htmlOutput("mergeData2")),
    tabPanel("Transform", htmlOutput("transform_data"),
             verbatimTextOutput("transform_summary"))
  )
})
