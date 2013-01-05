shinyUI(

  pageWithSidebar(
    headerPanel("Data Analysis Menu in Shiny"),
    
    sidebarPanel(
      wellPanel(
        conditionalPanel(condition = "input.dataset != 'choosefile'",
          selectInput(inputId = "tool", label = "Tool:", choices = toolChoices, selected = 'Data view')
          # selectInput(inputId = "tool", label = "Tool:", choices = toolChoices, selected = 'Kmeans clustering')
        ),
        uiOutput("dataloaded")
      ),

      conditionalPanel(condition = "input.tool == 'dataView'",
        wellPanel(
          fileInput("upload", "Load data (Rdata, CSV, Spss, or Stata format)"),
          uiOutput("packagedata")
        ),
        conditionalPanel(condition = "input.datatabs == 'Data' && input.dataset != 'choosefile'",
          wellPanel(
            uiOutput("rowsToShow"), 
            uiOutput("choose_columns")
          )
        )
      ),

      conditionalPanel(condition = "input.tool != 'dataView' ||  input.datatabs == 'Visualize'",
        conditionalPanel(condition = notInAnd,
          wellPanel(uiOutput("var1")),
          wellPanel(uiOutput("var2"))
        )
      ),

      conditionalPanel(condition = inOr,
        wellPanel(uiOutput("varinterdep"))
      )
    ),
    
    mainPanel(
      # includeHTML('shiny/lr.js'), # needed for livereload

      conditionalPanel(condition = "input.dataset != 'choosefile'",
        conditionalPanel(condition = "input.tool == 'dataView'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data", tableOutput("data")),
            tabPanel("Visualize", plotOutput("visualize")) 
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataView'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Summary", verbatimTextOutput("summary")), 
            tabPanel("Plots", plotOutput("plots", height = 1200)),
            tabPanel("Extra", verbatimTextOutput("extra")) 
          )
        )
      )
    )
  )
)