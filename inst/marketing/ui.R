# summary_panel <- function(tp = tabPanel("Summary", verbatimTextOutput("summary"))) {
#   tp
# }


shinyUI(

  pageWithSidebar(

    headerPanel("Radyant - Marketing analytics using Shiny"),

    sidebarPanel(

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
            HTML("<label>Load user data: (.rda | .csv | .sav | .dta)</label>"),
            actionButton("upload", "Choose a file"),
            # helpText("Loading user data disabled on Shiny-Server"),
            br(), br(),
            selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
          )
        ),
        conditionalPanel(condition = "input.datatabs == 'Data view' && input.datasets != ''",
          wellPanel(
            uiOutput("nrRows"), 
            uiOutput("columns"), 
            tags$style(type='text/css', "#columns { height: 250px; padding-bottom: 35px;}")
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
              downloadButton('downloadData', 'Save data'),
              tableOutput("dataviewer")
            ),
            tabPanel("Visualize", plotOutput("visualize", height = "1000px")),
            tabPanel("Transform", 
              tableOutput("transform_data"), br(),
              tableOutput("transform_summary")
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