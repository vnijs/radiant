# summary_panel <- function(tp = tabPanel("Summary", verbatimTextOutput("summary"))) {
#   tp
# }

shinyUI(

  pageWithSidebar(

    # headerPanel("Radyant - Marketing analytics using Shiny"),
    headerPanel(''),

    sidebarPanel(

      tags$head(
        tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
        tags$style(type="text/css", "select { max-width: 200px; }"),
        tags$style(type="text/css", "textarea { max-width: 185px; }"),
        tags$style(type="text/css", "text { width: 15px; !important }"),
        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
        tags$style(type='text/css', ".well { max-width: 310px; }"),
        tags$style(type='text/css', ".span4 { max-width: 310px; }")
        # tags$style(type='text/css', ".span8 { padding-left: 0%; }")
        # tags$style(type='text/css', ".container-fluid { width: 100%; }"),
        # tags$style(type="text/css", "html, body {width: 100%; height: 100%; overflow: hidden}"),
        # tags$style(type='text/css', ".tab-pane { width: 100%; }")
      ),

      includeHTML("www/navbar.html"),
      includeHTML("www/js/tools.js"),
      includeHTML('www/js/lr.js'), # needed for livereload

      wellPanel(
        # if there are no datasets available only show the UI to make data available
        conditionalPanel(condition = "input.datasets != ''",
          includeHTML('www/tools.html')
        ),
        uiOutput("datasets")
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'dataview'",
        conditionalPanel(condition = "input.datatabs == 'Data view'",
          wellPanel(
            HTML("<label>Load data: (.rda | .csv | .sav | .dta)</label>"),
            actionButton("upload", "Choose a file"),
            # helpText("Loading user data disabled on Shiny-Server"),
            br(), br(),
            selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
          )
        ),
        conditionalPanel(condition = "input.datatabs == 'Data view' && input.datasets != ''",
          wellPanel(
            uiOutput("columns"), 
            tags$style(type='text/css', "#columns { height: 200px; padding-bottom: 35px;}"),
            textInput("dv_select", "Subset (mpg > 20 & vs == 1)", ''),
            tags$style(type='text/css', "#dv_select { max-width: 185px; }"),
            uiOutput("nrRows")
          )
        ),
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
          uiOutput("ui_visualize")
        ),
        conditionalPanel(condition = "input.datatabs == 'Transform'",
          uiOutput("ui_transform")
        )
      ),
      conditionalPanel(condition = "input.tool != 'dataview'",
        # the appropriate analysis tool is called based on selection from
        # tools dropdown
        uiOutput("ui_analysis")
        # submitButton("Test")
      )
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.datasets != ''",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", 
              selectInput("saveAs", "", choices = c('rda','csv','dta'), selected = NULL, multiple = FALSE),
              tags$style(type='text/css', "#saveAs { width: 85px;}"),
              downloadButton('downloadData', 'Save data'),
              tags$style(type='text/css', "#downloadData { vertical-align: top; height: 18.5px; width: 70px;}"),
              tableOutput("dataviewer")
            ),
            tabPanel("Visualize", plotOutput("visualize", height = "100%")),
            tabPanel("Explore", HTML('<label>Explore your data using plyr, reshape, etc.<br>In progress. Check back soon.</label>')),
            tabPanel("Transform", 
              tableOutput("transform_data"), br(),
              # tableOutput("transform_summary")
              verbatimTextOutput("transform_summary")
            ),
            tabPanel("About", includeMarkdown("about.md"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Summary", verbatimTextOutput("summary")),
            # uiOutput("summary_panel"),
            # summary_panel(),
            tabPanel("Plots", plotOutput("plots", height = "100%")),
            tabPanel("Log", verbatimTextOutput('logwork'))
          )
        )
      )
    )
  )
)