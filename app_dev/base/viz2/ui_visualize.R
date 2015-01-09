library(dplyr)
library(magrittr)
library(ggvis)
library(shiny)

# source shared functions
source('../../dev_branch/base/radiant.R', local = TRUE)

varnames <- function() {
  letters
}


ui <- fluidPage(
  titlePanel("Viz2"),

  sidebarLayout(
    sidebarPanel(
      uiOutput("ui_viz_xvar")
    ),
    mainPanel(
      ggvisOutput("plot")
    )
  )
)

server <- function(input, output, session) {

  output$ui_viz_xvar <- renderUI({
    vars <- varnames()
    selectInput(inputId = "viz_xvar", label = "X-variable", choices = vars,
      selected = NULL, multiple = FALSE)
  })

  # A reactive subset of mtcars
  mtc <- reactive({ mtcars })

  # A simple visualisation. In shiny apps, need to register observers
  # and tell shiny where to put the controls
  mtc %>%
    ggvis(~wt, ~mpg) %>%
    layer_points() %>%
    bind_shiny("plot")

}

shinyApp(ui = ui, server = server)
