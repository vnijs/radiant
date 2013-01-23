# source('uiControls.R')
# "dataview", "singleMean", "compareMeans", "regression", "logistic", "hclustering", "kmeansClustering", "mds", "perceptualMap", "conjointDesign", "conjointAnalysis"

shinyUI(

  pageWithSidebar(

    headerPanel("Data Analysis for Marketing using Shiny"),
    
    sidebarPanel(

      includeHTML("www/js/damtools.js"),
      includeHTML('www/js/lr.js'), # needed for livereload

      wellPanel(
        # if there are no datasets available only show the UI to make data available
        conditionalPanel(condition = "input.datasets != ''",
          includeHTML('www/damtools.html')
        ),
        uiOutput("datasets")
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'dataview'",
        conditionalPanel(condition = "input.datatabs != 'Visualize'",
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
          wellPanel(
            uiOutput("vizvars1"),
            # uiOutput("vizvars2"), tags$style(type='text/css', "#vizvars2 { height: 250px; padding-bottom: 35px;}")
            uiOutput("vizvars2"),

            conditionalPanel(condition = "input.vizvars2 != ''",
              uiOutput("viz_color"),
              uiOutput("viz_facet_row"),
              uiOutput("viz_facet_col"),
              checkboxInput('viz_jitter', 'Jitter', value = FALSE),
              checkboxInput('viz_smooth', 'Smooth', value = TRUE)
            )
          )
        )
      ),

      conditionalPanel(condition = "input.tool != 'dataview'",
        uiOutput("analysis_ui_controls")
        # submitButton("Test")
      )
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.datasets != ''",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", tableOutput("dataviewer")),
            tabPanel("Visualize", plotOutput("visualize", width = "100%", height = "100%")),
            # tabPanel("Transform", tableOutput("transform")),      # once transform has been implement use tableOutput
            tabPanel("Transform", verbatimTextOutput("transform")),
            tabPanel("About", includeMarkdown("about.md"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Summary", verbatimTextOutput("summary")), 
            tabPanel("Plots", plotOutput("plots", width = "100%", height = "100%")),
            # tabPanel("Plots", plotOutput("plots", height = "1200px")),
            tabPanel("Log", verbatimTextOutput('logwork'))
          )
        )
      )
    )
  )
)