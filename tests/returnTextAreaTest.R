# returnTextAreaInput <- function(inputId, label, value = "") {
#   tagList(tags$label(label, `for` = inputId),
#           tags$input(id = inputId, type = "text", value = value,
#                      class="returnTextArea"))
# }

returnTextAreaInput <- function(inputId, label, value = "") {
  tagList(tags$label(label, `for` = inputId),br(),
		tags$textarea(class="form-control returnTextArea",
		              id=inputId, type = "text", value = value, rows="3")
  )
}

ui <- basicPage(
    includeScript("~/gh/radiant_dev/inst/base/www/js/returnTextAreaBinding.js"),
    returnTextAreaInput("testInput","Select stuff:",""),
    textOutput("text_out")
)

server <- function(input, output, session) {

  output$text_out <- renderPrint({
    print("Press enter or focusout to update --- ")
    input$testInput
  })

}

shinyApp(ui = ui, server = server)
