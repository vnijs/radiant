library(dplyr)
library(magrittr)
library(ggvis)
library(shiny)

# source shared functions
source('radiant.R', local = TRUE)

#######################################
# Vizualize data
#######################################
output$uiVizvars1 <- renderUI({
  vars <- varnames()
  selectInput(inputId = "vizvars1", label = "X-variable", choices = vars,
    selected = state_multvar("vizvars1",vars), multiple = input$viz_multiple == 'multiple',
    selectize = input$viz_multiple == 'single')
})

shinyUI(navbarPage("Radiant - Base", id = "nav_radiant", inverse = TRUE, collapsable = TRUE,

  tabPanel("Data", uiOutput('data_ui_and_tabs')),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Radiant help", uiOutput("help_base")),
    tabPanel("Radiant videos", withMathJax(), "../base/tools/app/videos_quant.Rmd" %>%
               inclRmd %>% HTML),
    # do NOT remove withMathJax(), load-files breaks!
    tabPanel("About", withMathJax(), "../base/tools/app/about.Rmd" %>% inclRmd %>% HTML)
  )
))




  tabList(
    includeCSS("../base/www/style.css"),
    includeScript("../base/www/js/jquery-ui.custom.min.js"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("uiDatasets"),
        conditionalPanel(condition = "input.datatabs == 'Manage'", uiOutput("ui_Manage")),
        conditionalPanel(condition = "input.datatabs == 'View'", uiOutput("ui_View")),
        conditionalPanel(condition = "input.datatabs == 'Visualize'", uiOutput("ui_Visualize")),
        conditionalPanel(condition = "input.datatabs == 'Explore'", uiOutput("ui_Explore")),
        conditionalPanel(condition = "input.datatabs == 'Merge'", uiOutput("ui_Merge")),
        conditionalPanel(condition = "input.datatabs == 'Transform'", uiOutput("ui_Transform")),
        conditionalPanel(condition = "input.datatabs == 'Filter'", uiOutput("ui_Filter"))
      ),
      mainPanel(id = "datatabs",
        uiOutput("tabs_data")
      )
    )
  )


ui <- bootstrapPage(
  #   ggvisOutput("plot1"),
  #   ggvisOutput("plot2")
  htmlOutput("grid_ggvis")
)

server <- function(input, output, session) {

  lb <- linked_brush(keys = cocaine$id, "red")

  cocaine %>%
    ggvis(~weight, ~price, key := ~id) %>%
    layer_points(fill := lb$fill, fill.brush := "red", opacity := 0.3) %>%
    lb$input() %>%
    set_options(width = 300, height = 300) %>%
    bind_shiny("plot1") # Very important!


  # A subset of cocaine, of only the selected points
  selected <- lb$selected
  cocaine_selected <- reactive({
    cocaine[selected(), ]
  })

  cocaine %>%
    ggvis(~potency) %>%
    layer_histograms(width = 5, boundary = 0) %>%
    add_data(cocaine_selected) %>%
    layer_histograms(width = 5, boundary = 0, fill := "#dd3333") %>%
    set_options(width = 300, height = 300) %>%
    bind_shiny("plot2")

  output$grid_ggvis <- renderUI({

    p1 <- ggvisOutput("plot1")
    p2 <- ggvisOutput("plot2")

    # no graphs shown
    # HTML(paste0("<table><td>",p1,"</td><td>",p2,"</td></table>"))

    # clunky but seems to work
    html1 <- HTML("<table><td>")
    html2 <- HTML("</td><td>")
    html3 <- HTML("</td></table>")
    list(html1,p1,html2,p2,html3)
  })
}

shinyApp(ui = ui, server = server)



output$data_ui_and_tabs <- renderUI({
  tabList(
    includeCSS("../base/www/style.css"),
    includeScript("../base/www/js/jquery-ui.custom.min.js"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("uiDatasets"),
        conditionalPanel(condition = "input.datatabs == 'Manage'", uiOutput("ui_Manage")),
        conditionalPanel(condition = "input.datatabs == 'View'", uiOutput("ui_View")),
        conditionalPanel(condition = "input.datatabs == 'Visualize'", uiOutput("ui_Visualize")),
        conditionalPanel(condition = "input.datatabs == 'Explore'", uiOutput("ui_Explore")),
        conditionalPanel(condition = "input.datatabs == 'Merge'", uiOutput("ui_Merge")),
        conditionalPanel(condition = "input.datatabs == 'Transform'", uiOutput("ui_Transform")),
        conditionalPanel(condition = "input.datatabs == 'Filter'", uiOutput("ui_Filter"))
      ),
      mainPanel(id = "datatabs",
        uiOutput("tabs_data")
      )
    )
  )
})
