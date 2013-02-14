getTool <- function(inputId, selected = 'dataview') {
  tagList(
    singleton(tags$head(tags$script(src = "js/navbar.js"))),
    tags$html(id=inputId, includeHTML('www/navbar.html'))
  )
}

shinyUI(

  pageWithSidebar(

    # Using a navbar rather than headerPanel to display app title
    headerPanel(''),

    sidebarPanel(

      tags$head(
        tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
        tags$style(type="text/css", "select { max-width: 200px; }"),
        tags$style(type="text/css", "textarea { max-width: 185px; }"),
        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
        tags$style(type='text/css', ".well { padding: 12px; margin-bottom: 5px; max-width: 280px; }"),
        tags$style(type='text/css', ".span4 { max-width: 280px; }")
      ),

      includeHTML("www/js/tools.js"),
      includeHTML('www/js/lr.js'), 
      getTool("tool"),

      wellPanel(
        uiOutput("datasets")
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'dataview'",
        conditionalPanel(condition = "input.datatabs == 'View'",
          wellPanel(
            HTML("<label>Load data: (.rda | .csv | .sav | .dta)</label>"),
            actionButton("upload", "Choose a file"),
            br(), br(),
            selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
          )
        ),
        conditionalPanel(condition = "input.datatabs == 'View' && input.datasets != ''",
          wellPanel(
            uiOutput("columns"), 
            tags$style(type='text/css', "#columns { height: 200px; padding-bottom: 35px;}"),
            # textInput("dv_select", "Subset (e.g., mpg > 20 & vs == 1)", ''), actionButton("sub", "Go"),
            textInput("dv_select", "Subset (e.g., mpg > 20 & vs == 1)", ''), actionButton("sub_select", "Go"),
            tags$style(type='text/css', "#dv_select { max-width: 135px; }"),
            tags$style(type='text/css', "#sub_select { vertical-align: top; width: 45px; }"),
            # tags$style(type='text/css', "#dv_select { max-width: 185px; }"),
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
            tabPanel("View", 
              selectInput("saveAs", "", choices = c('rda','csv','dta'), selected = NULL, multiple = FALSE),
              tags$style(type='text/css', "#saveAs { width: 85px;}"),
              downloadButton('downloadData', 'Save data'),
              tags$style(type='text/css', "#downloadData { vertical-align: top; height: 18.5px; width: 70px;}"),
              tableOutput("dataviewer")
            ),
            tabPanel("Transform", 
              tableOutput("transform_data"), br(),
              verbatimTextOutput("transform_summary")
            ),
            tabPanel("Summarize", HTML('<label>Summarize and explore your data using plyr, reshape, etc.<br>In progress. Check back soon.</label>')),
            tabPanel("Visualize", plotOutput("visualize", height = "100%")),
            tabPanel("About", includeMarkdown("about.md"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Summary", verbatimTextOutput("summary")),
            # tabPanel("Summary", uiOutput("summary")),
            # Use Summarize / Vizualize labels here?
            tabPanel("Plots", plotOutput("plots", height = "100%")),
            tabPanel("Log", verbatimTextOutput('logwork'))
          )
        )
      )
    )
  )
)