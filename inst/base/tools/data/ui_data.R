#######################################
# Shiny interface for data functions
#######################################

## show error message from filter dialog
output$ui_filter_error <- renderUI({
  if (is_empty(r_data$filter_error)) return()
  helpText(r_data$filter_error)
})

## data ui and tabs
output$ui_data <- renderUI({
  tagList(
    includeCSS(file.path(r_path,"base/www/style.css")),
    # includeScript(file.path(r_path,"base/www/js/returnTextAreaBinding.js")),
    # includeScript(file.path(r_path,"base/www/js/returnTextInputBinding.js")),
    sidebarLayout(
      sidebarPanel(
        ## based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        wellPanel(
          uiOutput("ui_datasets"),
          conditionalPanel("input.tabs_data != 'Manage'",
            checkboxInput('show_filter', 'Filter (e.g., price > 5000)', value = state_init("show_filter",FALSE)),
            conditionalPanel("input.show_filter == true",
              returnTextAreaInput("data_filter", label = "", value = state_init("data_filter")),
              uiOutput("ui_filter_error")))
        ),
        conditionalPanel("input.tabs_data == 'Manage'", uiOutput("ui_Manage")),
        conditionalPanel("input.tabs_data == 'View'", uiOutput("ui_View")),
        conditionalPanel("input.tabs_data == 'Visualize'", uiOutput("ui_Visualize")),
        conditionalPanel("input.tabs_data == 'Pivot'",uiOutput("ui_Pivotr")),
        conditionalPanel("input.tabs_data == 'Explore'",
                         uiOutput("ui_Explore")),
        conditionalPanel("input.tabs_data == 'Transform'", uiOutput("ui_Transform")),
        conditionalPanel("input.tabs_data == 'Combine'", uiOutput("ui_Combine"))),
      mainPanel(
        tabsetPanel(id = "tabs_data",
          tabPanel("Manage", htmlOutput("htmlDataExample"),
            conditionalPanel("input.man_add_descr == false", uiOutput("dataDescriptionHTML")),
            conditionalPanel("input.man_add_descr == true", uiOutput("dataDescriptionMD"))),
          tabPanel("View",
                   downloadLink("dl_view_tab", "", class = "fa fa-download alignright"),
                   DT::dataTableOutput("dataviewer")),
          # tabPanel("View", DT::dataTableOutput("dataviewer"), verbatimTextOutput("tbl_state")),
          tabPanel("Visualize",
                   plot_downloader(".visualize", width = viz_plot_width(), height = viz_plot_height(), pre = ""),
                   plotOutput("visualize", width = "100%", height = "100%")),
          tabPanel("Pivot",
                   conditionalPanel("input.pvt_tab == true",
                     downloadLink("dl_pivot_tab", "", class = "fa fa-download alignright"),
                     DT::dataTableOutput("pivotr")
                   ),
                   conditionalPanel("input.pvt_chi2 == true", htmlOutput("pivotr_chi2")),
                   conditionalPanel("input.pvt_plot == true",
                     HTML("<br><br>"),
                     plot_downloader("pivot", width = pvt_plot_width(), height = pvt_plot_height()),
                     plotOutput("plot_pivot", width = "100%", height = "100%")
                   )
          ),
          tabPanel("Explore",
                   downloadLink("dl_explore_tab", "", class = "fa fa-download alignright"),
                   DT::dataTableOutput("explorer")),
          tabPanel("Transform",
                   htmlOutput("transform_data"),
                   verbatimTextOutput("transform_summary"),
                   uiOutput("ui_tr_log")),
          tabPanel("Combine", htmlOutput("cmb_data1"), htmlOutput("cmb_data2"),
                   htmlOutput("cmb_possible"), htmlOutput("cmb_data"))
          # tabPanel("Generate", HTML("<h3>Generate input data for simulation and prediction</h3>")),
          # , selected = ifelse (is_empty(r_url$tab), "Manage", r_url$tab)
        )
      )
    )
  )
})
