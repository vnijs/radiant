library(shiny)
ui <- bootstrapPage(

  selectInput(inputId = "data1", label = "",
              choices = LETTERS[1:5],
    					selected = "A",
    					multiple = TRUE),

  selectInput(inputId = "data2", label = "",
              choices = LETTERS[1:5],
              selected = "A",
              multiple = TRUE, selectize = FALSE),

  tags$textarea(id="old_look",
                rows="3"),

  tags$textarea(class="form-control",
                id="new_look",
                rows="3")
)

server <- function(input, output, session) {
#   if(exists("session")) print("yes there is one")
#   print(session)
}

shinyApp(ui = ui, server = server)
