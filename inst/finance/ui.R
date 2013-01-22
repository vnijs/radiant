source('garch.R', local = TRUE)

nasdaq_file <- read.csv('data/nasdaq-company-list.csv', stringsAsFactors = FALSE)
symbol_list <- c("",nasdaq_file[,'Symbol'])
names(symbol_list) <- c("",nasdaq_file[,'Name'])
symbol_list <- as.list(symbol_list)

shinyUI(

  pageWithSidebar(

    headerPanel("Data Analysis for Finance using Shiny"),
    
    sidebarPanel(

      includeHTML("www/js/damtools.js"),
      # includeHTML('www/js/lr.js'), # needed for livereload

      wellPanel(
        # if there are no datasets available only show the UI to make data available
        conditionalPanel(condition = "input.datasets != 'choosefile'",
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
          selectInput("yahooData", label = "Fetch data from Yahoo:", choices = symbol_list, selected = "", multiple = FALSE)
        ),

        wellPanel(
          uiOutput("nrRows"), 
          conditionalPanel(condition = "input.datatabs == 'Data view' && input.datasets != ''",
            uiOutput("columns"), tags$style(type='text/css', "#columns { height: 250px; padding-bottom: 35px;}")
          )
        ),
     
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
          selectInput(inputId = "chart_type", label = "Chart type", choices = c("Candlestick" = "candlesticks", "Line" = "line")),
          checkboxInput(inputId = "log_data", label = "Log data", value = FALSE)
        )
      ),


      conditionalPanel(condition = "input.tool != 'dataview'",
        controlsFor_garch()
      )
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.datasets != 'choosefile'",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", tableOutput("dataviewer")),
            tabPanel("Visualize", plotOutput("visualize")),
            tabPanel("About", includeMarkdown("about.md"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Summary", verbatimTextOutput("summary")), 
            tabPanel("Plots", plotOutput("plots", height = "1200px"))
          )
        )
      )
    )
  )
)