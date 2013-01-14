shinyUI(

  pageWithSidebar(
    headerPanel("Data Analysis Menu in Shiny"),
    
    sidebarPanel(
      wellPanel(
        conditionalPanel(condition = "input.datasets != 'choosefile'",
          selectInput(inputId = "tool", label = "Tool:", choices = toolChoices, selected = 'Data view')
        ),
        uiOutput("datasets")
      ),

      conditionalPanel(condition = "input.tool == 'dataview'",
        wellPanel(
          fileInput("upload", "Load data (Rdata, CSV, Spss, or Stata format)"),
          # actionButton("upload", "Load data (Rdata, CSV, Spss, or Stata format)")
          # helpText("Loading user data disabled on Glimmer"),
          uiOutput("packData")
        ),
        conditionalPanel(condition = "input.datatabs == 'Data view' && input.datasets != 'choosefile'",
          wellPanel(
            uiOutput("nrRows"), 
            uiOutput("columns")
          )
        )
      ),

      conditionalPanel(condition = "input.tool != 'dataview' ||  input.datatabs == 'Visualize'",
        conditionalPanel(condition = notInAnd,
          wellPanel(uiOutput("var1")),
          wellPanel(uiOutput("var2"),tags$style(type='text/css', "#var2 { height: 200px;}"))
        )
      ),

      conditionalPanel(condition = inOr,
        wellPanel(
          uiOutput("nrClus"),
          uiOutput("varinterdep"), tags$style(type='text/css', "#varinterdep { height: 200px;}")
        )
      ),

      conditionalPanel(condition = inOrChange,
        conditionalPanel(condition = "input.analysistabs == 'Summary'",
          # uiOutput("addoutput"),
          actionButton("abutton", "Press the button")
        )
      )

      # Should be using an action button rather than a check box see above to add 
      # variables to the dataset. Doesn't work right just yet so leaving this 
      # here for now.
      # conditionalPanel(condition = inOrChange,
      #   conditionalPanel(condition = "input.analysistabs == 'Summary'",
      #     conditionalPanel(condition = "input.tool == 'regression'",
      #       actionButton("addoutput", "Save residuals (see Data view)")
      #     ),
      #     conditionalPanel(condition = "input.tool == 'kmeansClustering'",
      #       actionButton("addoutput", "Save cluster membership (see Data view)")
      #     )
      #   )
      # )
    ),
    
    mainPanel(
      includeHTML('lr.js'), # needed for livereload
      conditionalPanel(condition = "input.datasets != 'choosefile'",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", tableOutput("dataviewer")),
            tabPanel("Visualize", plotOutput("visualize")),
            # tabPanel("Transform", tableOutput("transform")),
            tabPanel("Transform", verbatimTextOutput("transform")),
            tabPanel("Log", verbatimTextOutput('logwork')),
            tabPanel("About", includeHTML("about.html"))
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