#######################################
# Shiny interface for data functions
#######################################

output$ui_filter_error <- renderUI({
  if (is_empty(r_data$filter_error)) return()
  helpText(r_data$filter_error)
})

# data ui and tabs
output$ui_data <- renderUI({
  tagList(
    includeCSS(file.path(r_path,"base/www/style.css")),
    includeScript(file.path(r_path,"base/www/js/returnTextAreaBinding.js")),
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
        # conditionalPanel("input.datatabs == 'View_old'",uiOutput("ui_View_old")),
        conditionalPanel("input.datatabs == 'Visualize'", uiOutput("ui_Visualize")),
        conditionalPanel("input.datatabs == 'Pivot'",uiOutput("ui_Pivot")),
        conditionalPanel("input.datatabs == 'Explore'", uiOutput("ui_Explore")),
        conditionalPanel("input.datatabs == 'Transform'", uiOutput("ui_Transform")),
        conditionalPanel("input.datatabs == 'Combine'", uiOutput("ui_Combine"))),
      mainPanel(
        tabsetPanel(id = "datatabs",
          tabPanel("Manage", htmlOutput("htmlDataExample"),
            conditionalPanel("input.man_add_descr == false", uiOutput("dataDescriptionHTML")),
            conditionalPanel("input.man_add_descr == true", uiOutput("dataDescriptionMD"))),
          tabPanel("View", DT::dataTableOutput("dataviewer")),
          # tabPanel("View_old", shiny::dataTableOutput("dataviewer_old")),
          tabPanel("Visualize",plotOutput("visualize", width = "100%", height = "100%")),
          tabPanel("Pivot", rpivotTable::rpivotTableOutput("pivotData")),
          tabPanel("Explore", verbatimTextOutput("expl_summary"), plotOutput("expl_plots", width = "100%", height = "100%")),
          tabPanel("Transform", htmlOutput("transform_data"), verbatimTextOutput("transform_summary")),
          tabPanel("Combine", htmlOutput("cmb_possible"), htmlOutput("cmb_data1"),
                   htmlOutput("cmb_data2"), htmlOutput("cmb_data"))
        )
      )
    )
  )
})

# to add
# tabPanel("Generate", HTML("<h3>Generate input data for simulation and prediction</h3>")),
