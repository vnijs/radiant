shinyUI(

  pageWithSidebar(

    headerPanel("Data Analysis Menu using Shiny"),
    
    sidebarPanel(

      includeHTML("www/js/damtools.js"),
      includeHTML('www/js/lr.js'), # needed for livereload

      wellPanel(
        # if there are no datasets available only show the UI to make data available
        conditionalPanel(condition = "input.datasets != 'choosefile'",
          #selectInput(inputId = "tool", label = "Tool:", choices = toolChoices, selected = 'Data view')
          includeHTML('www/damtools.html')
        ),
        uiOutput("datasets")
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'dataview'",
        wellPanel(
          HTML("<label>Load user data: (.rda | .csv | .sav | .dta)</label>"),
          actionButton("upload", "Choose a file"),
          # helpText("Loading user data disabled on Shiny-Server"),
          br(), br(),
          selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
        ),

        conditionalPanel(condition = "input.datatabs == 'Data view' && input.datasets != 'choosefile'",
          wellPanel(
            uiOutput("nrRows"), 
            uiOutput("columns"), 
            tags$style(type='text/css', "#columns { height: 250px; padding-bottom: 35px;}")
          )
        ),
     
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
          wellPanel(
            uiOutput("varview1"),
            uiOutput("varview2"),
            tags$style(type='text/css', "#varview2 { height: 250px; padding-bottom: 35px;}")
          )
        )
      ),

      conditionalPanel(condition = "input.tool != 'dataview'",

        conditionalPanel(condition = notInInterdep,
          wellPanel(
            uiOutput("var1"),
            conditionalPanel(condition = notInSingle,
              uiOutput("var2"),tags$style(type='text/css', "#var2 { height: 200px; padding-bottom: 35px;}")
            ),
            conditionalPanel(condition = "input.tool == 'regression'",
              actionButton("saveres", "Save residuals (see Data view)")
            )
          ),


          conditionalPanel(condition = "input.tool != 'regression'",
            wellPanel(
              uiOutput("alternative"),
              sliderInput('sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01),
              conditionalPanel(condition = "input.tool == 'singleMean'",
                numericInput("compValue", "Comparison value:", 0)
              )
            )
          )
        ),

        conditionalPanel(condition = inInterdep,
          wellPanel(
            uiOutput("nrClus"),
            uiOutput("varinterdep"), tags$style(type='text/css', "#varinterdep { height: 250px; padding-bottom: 35px;}"),
            actionButton("saveclus", "Save cluster membership (see Data view)")
          )
        )
      )
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.datasets != 'choosefile'",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", tableOutput("dataviewer")),
            tabPanel("Visualize", plotOutput("visualize")),
            # tabPanel("Transform", tableOutput("transform")),      # once transform has been implement use tableOutput
            tabPanel("Transform", verbatimTextOutput("transform")),
            tabPanel("Log", verbatimTextOutput('logwork')),
            tabPanel("About", includeMarkdown("about.md"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Summary", verbatimTextOutput("summary")), 
            tabPanel("Plots", plotOutput("plots", height = 1200)),
            tabPanel("Extra", verbatimTextOutput("extra")),
            tabPanel("Log", verbatimTextOutput("log")) 
          )
        )
      )
    )
  )
)