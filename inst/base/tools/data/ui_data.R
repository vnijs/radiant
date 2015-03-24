#######################################
# Shiny interface for data functions
#######################################

output$ui_filter_error <- renderUI({
  if(is_empty(r_data$filter_error)) return()
  helpText(r_data$filter_error)
})

# data ui and tabs
output$ui_data <- renderUI({
  list(
    includeCSS("../base/www/style.css"),
    includeScript("../base/www/js/jquery-ui.custom.min.js"),
    includeScript("../base/www/js/returnTextAreaBinding.js"),
    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        wellPanel(
          uiOutput("ui_datasets"),
          conditionalPanel("input.datatabs != 'Manage'",
            checkboxInput('show_filter', 'Filter (e.g., price > 5000)', value = state_init("show_filter",FALSE)),
            conditionalPanel("input.show_filter == true",
              returnTextAreaInput("data_filter", label = "", value = state_init("data_filter")),
              uiOutput("ui_filter_error")))
        ),
        conditionalPanel("input.datatabs == 'Manage'", uiOutput("ui_Manage")),
        conditionalPanel("input.datatabs == 'View'",uiOutput("ui_View")),
        conditionalPanel("input.datatabs == 'Visualize'", uiOutput("ui_Visualize")),
        # conditionalPanel("input.datatabs == 'Pivot'",uiOutput("ui_Pivot")),
        conditionalPanel("input.datatabs == 'Explore'", uiOutput("ui_Explore")),
        conditionalPanel("input.datatabs == 'Transform'", uiOutput("ui_Transform")),
        conditionalPanel("input.datatabs == 'Merge'", uiOutput("ui_Merge"))),
      mainPanel(
        uiOutput("datatabs")
      )
    )
  )
})

# data tabs
output$datatabs <- renderUI({
  tabsetPanel(id = "datatabs",
    tabPanel("Manage", htmlOutput("htmlDataExample"),
      conditionalPanel("input.man_add_descr == false", uiOutput("dataDescriptionHTML")),
      conditionalPanel("input.man_add_descr == true", uiOutput("dataDescriptionMD"))),
    tabPanel("View", dataTableOutput("dataviewer")),
    tabPanel("Visualize",plotOutput("visualize", width = "100%", height = "100%")),
    # tabPanel("Pivot", rpivotTable::rpivotTableOutput("pivotData")),
    tabPanel("Explore", verbatimTextOutput("expl_summary"), plotOutput("expl_plots", width = "100%", height = "100%")),
    tabPanel("Transform", htmlOutput("transform_data"), verbatimTextOutput("transform_summary")),
    tabPanel("Merge", htmlOutput("merge_possible"), htmlOutput("mergedata1"), htmlOutput("mergedata2"))
  )
})

# to add
# tabPanel("View", DT::dataTableOutput("dataviewer")),
# tabPanel("Generate", HTML("<h3>Generate input data for simulation and prediction</h3>")),
