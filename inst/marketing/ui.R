shinyUI(

  pageWithSidebar(

    # Using a navbar rather than headerPanel to display app title
    headerPanel(''),

    sidebarPanel(

      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="style.css"),
        tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
      ),

      includeHTML("www/js/sel2.js"),
      includeHTML('www/js/lr.js'), 
      getTool("tool"),

      wellPanel(
        uiOutput("datasets")
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'dataview'",
        conditionalPanel(condition = "input.datatabs == 'View'",
          wellPanel(

            radioButtons(inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", ".xls" = "xls"), selected = ".rda"),
            conditionalPanel(condition = "input.dataType != 'xls'",
              conditionalPanel(condition = "input.dataType == 'csv'",
                checkboxInput('header', 'Header', TRUE),
                radioButtons('sep', '', c(Comma=',', Semicolon=';', Tab='\t'), 'Comma')
              ),
              fileInput('uploadfile', '')
            ),
            conditionalPanel(condition = "input.dataType == 'xls'",
              HTML("<label>Copy-and-paste data from Excel</label>"),
              tags$textarea(id="xls_paste", rows=3, cols=40, "")
            )
            # selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
          )
        ),
        conditionalPanel(condition = "input.datatabs == 'View' && input.datasets != ''",
          wellPanel(
            uiOutput("columns"), 
            tags$style(type='text/css', "#columns { height: 200px; padding-bottom: 35px;}"),
            textInput("dv_select", "Subset (e.g., mpg > 20 & vs == 1)", ''), actionButton("sub_select", "Go"),
            tags$style(type='text/css', "#dv_select { max-width: 135px; }"),
            tags$style(type='text/css', "#sub_select { vertical-align: top; width: 45px; }"),
            uiOutput("nrRows")
          ),
          helpModal('View',includeRmd("tools/help/example.Rmd"))
        ),
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
          uiOutput("ui_visualize"),
          helpModal('Visualize',includeRmd("tools/help/example.Rmd"))
        ),
        conditionalPanel(condition = "input.datatabs == 'Transform'",
          uiOutput("ui_transform"),
          helpModal('Transform',includeRmd("tools/help/example.Rmd"))
        )
      ),
      conditionalPanel(condition = "input.tool != 'dataview'",
        # the appropriate analysis code called based on the selection from the navbar
        uiOutput("ui_analysis")
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
            tabPanel("About", includeRmd("about.Rmd"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'dataview'",
          tabsetPanel(id = "analysistabs",
            tabPanel("Summary", verbatimTextOutput("summary")),
            tabPanel("Plots", plotOutput("plots", height = "100%"))
          )
        )
      )
    )
  )
)