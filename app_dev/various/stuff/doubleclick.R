library(shiny)

ui = bootstrapPage(
  tags$button(id = 'mybutton', 'button', class='btn btn-primary', value = 0),
  textOutput('x'),
  # when button is double clicked increase the value by one
  # and update the input variable x
  tags$script("
    $('#mybutton').on('dblclick', function(){
      var val = +this.value
      this.value = val + 1
      Shiny.onInputChange('x', this.value)
      console.log(this.value)
    })
  ")
)

server = function(input, output, session){
  output$x <- renderText({
    input$x
  })
}

runApp(list(ui = ui, server = server))
