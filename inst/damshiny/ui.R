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
          wellPanel(uiOutput("var2"))
        )
      ),

      conditionalPanel(condition = inOr,
        wellPanel(uiOutput("varinterdep"))
      )
    ),
    
    mainPanel(
      includeHTML('shiny/lr.js'), # needed for livereload
      conditionalPanel(condition = "input.datasets != 'choosefile'",
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Data view", tableOutput("dataviewer")),
            tabPanel("Visualize", plotOutput("visualize")) 
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
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

# shinyUI(

#   pageWithSidebar(
#     headerPanel("Data Analysis Menu in Shiny"),
    
#     sidebarPanel(
#       wellPanel(
#         conditionalPanel(condition = "input.datasets != 'choosefile'",
#           selectInput(inputId = "tool", label = "Tool:", choices = toolChoices, selected = 'Data view')
#         ),
#         uiOutput("dataview")
#       ),

#       conditionalPanel(condition = "input.tool == 'dataview'",
#         wellPanel(
#           fileInput("upload", "Load data (Rdata, CSV, Spss, or Stata format)"),
#           uiOutput("packData")
#         ),
#         conditionalPanel(condition = "input.datatabs == 'Data view' && input.datasets != 'choosefile'",
#           wellPanel(
#             uiOutput("nrRows"), 
#             uiOutput("columns")
#           )
#         )
#       ),

#       conditionalPanel(condition = "input.tool != 'dataview' ||  input.datatabs == 'Visualize'",
#         conditionalPanel(condition = notInAnd,
#           wellPanel(uiOutput("var1")),
#           wellPanel(uiOutput("var2"))
#         )
#       ),

#       conditionalPanel(condition = inOr,
#         wellPanel(uiOutput("varinterdep"))
#       )
#     ),
    
#     mainPanel(
#       includeHTML('shiny/lr.js'), # needed for livereload
#       conditionalPanel(condition = "input.datasets != 'choosefile'",
#         conditionalPanel(condition = "input.tool == 'dataview'", 
#           tabsetPanel(id = "datatabs",
#             tabPanel("Data view", tableOutput("dataview")),
#             tabPanel("Visualize", plotOutput("visualize")) 
#           )
#         ),
#         conditionalPanel(condition = "input.tool != 'dataview'",
#           tabsetPanel(id = "analysistabs",
#             tabPanel("Summary", verbatimTextOutput("summary")), 
#             tabPanel("Plots", plotOutput("plots", height = 1200)),
#             tabPanel("Extra", verbatimTextOutput("extra")) 
#           )
#         )
#       )
#     )
#   )
# )