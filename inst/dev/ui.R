shinyUI(

  pageWithSidebar(

    headerPanel("Data Analysis Menu using Shiny"),
    
    sidebarPanel(

      includeHTML("www/js/damtools.js"),

      wellPanel(
        # if there are no datasets available only show the UI to make data available
        conditionalPanel(condition = "input.datasets != 'choosefile'",
          # selectInput(inputId = "tool", label = "Tool:", choices = toolChoices, selected = 'Data view')
          selectInput(inputId = "tool", label = "Tool:", choices = toolChoices, selected = 'EDAT - Compare means')
        ),
        br(),
        uiOutput("datasets")
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'dataview'",
        wellPanel(
          HTML("<label>Load user data:<br>(.rda | .csv | .sav | .dta)</label>"),
          actionButton("upload", "Choose a file"),
          # helpText("Loading user data disabled on Shiny-Server"),
          br(), br(),
          # uiOutput("packData")
          selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
        )
      ),

      conditionalPanel(condition = "input.tool == 'dataview' && input.datatabs == 'Data view' && input.datasets != 'choosefile'",
          wellPanel(
            uiOutput("nrRows"), 
            uiOutput("columns")
          )
      ),
     
      conditionalPanel(condition = "input.tool == 'dataview' && input.datatabs == 'Visualize'",
          wellPanel(uiOutput("varview1")), 
          wellPanel(uiOutput("varview2"),tags$style(type='text/css', "#varview2 { height: 250px; padding-bottom: 35px;}"))
      ),

      conditionalPanel(condition = "input.tool != 'dataview'",
        conditionalPanel(condition = notInAnd,
          wellPanel(uiOutput("var1")), 
          wellPanel(uiOutput("var2"),tags$style(type='text/css', "#var2 { height: 250px; padding-bottom: 35px;}"))
        )
      ),

      conditionalPanel(condition = inOr,
        wellPanel(
          uiOutput("nrClus"),
          uiOutput("varinterdep"), tags$style(type='text/css', "#varinterdep { height: 250px; padding-bottom: 35px;}")
        )
      ),

      conditionalPanel(condition = "input.analysistabs == 'Summary'",
        conditionalPanel(condition = "input.tool == 'regression'",
          actionButton("saveres", "Save residuals (see Data view)")
        ),
        conditionalPanel(condition = "input.tool == 'kmeansClustering'",
          actionButton("saveclus", "Save cluster membership (see Data view)")
        )
      )
    ),
    
    mainPanel(
      includeHTML('www/js/lr.js'), # needed for livereload
      conditionalPanel(condition = "input.datasets != 'choosefile'",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", tableOutput("dataviewer")),
            tabPanel("Visualize", plotOutput("visualize")),
            # tabPanel("Transform", tableOutput("transform")),      # once transform has been implement use tableOutput
            tabPanel("Transform", verbatimTextOutput("transform")),
            tabPanel("Log", verbatimTextOutput('logwork')),
            tabPanel("About", includeHTML("about.html"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Plots", plotOutput("plots", height = 1200)),
            tabPanel("Summary", verbatimTextOutput("summary")), 
            tabPanel("Extra", verbatimTextOutput("extra")),
            tabPanel("Log", verbatimTextOutput("log")) 
          )
        )
      )
    )
  )
)