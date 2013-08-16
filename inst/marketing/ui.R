getTool <- function(inputId) {
  tagList(
    tags$head(tags$script(src = "js/navbar.js")),
    tags$html(includeHTML('www/navbar.html'))
  )
}

helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'), 
  trigger=c('click', 'hover', 'focus', 'manual')) {

  tagList(
    singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
    # singleton(tags$head(tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'))),
    tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1], 
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="icon-question-sign"))
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
        tags$style(type='text/css', ".span4 { max-width: 280px; }"),
        tags$style(type='text/css', ".popover { html: true; width: 600px; position: relative; top: -75px !important; left: 320px !important; }"),
        tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
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
            # HTML("<label>Load data: (.rda | .csv | .sav | .dta)</label>"),
            # actionButton("upload", "Choose a file"), br(), br(),

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
          # helpPopup('View',includeRmd("tools/help/view.Rmd"))
          helpPopup('View',includeHTML("tools/help/view.html"))
        ),
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
          uiOutput("ui_visualize"),
          helpPopup('Visualize',includeRmd("tools/help/view.Rmd"))
        ),
        conditionalPanel(condition = "input.datatabs == 'Transform'",
          uiOutput("ui_transform"),
          helpPopup('Transform',includeRmd("tools/help/view.Rmd"))
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