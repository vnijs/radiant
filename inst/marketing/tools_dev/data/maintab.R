# how much of the main panel could/should be separated out?

    mainPanel(
      conditionalPanel(condition = "input.datasets != ''",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", 
              selectInput("saveAs", "", choices = c('rda','csv','dta'), selected = NULL, multiple = FALSE),
              tags$style(type='text/css', "#saveAs { width: 85px;}"),
              downloadButton('downloadData', 'Save data'),
              tags$style(type='text/css', "#downloadData { vertical-align: top; width: 70px;}"),
              # tags$style(type='text/css', "#downloadData { padding-bottom: 35px;}"),
              tableOutput("dataviewer")
            ),
            tabPanel("Visualize", plotOutput("visualize", height = "100%")),
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