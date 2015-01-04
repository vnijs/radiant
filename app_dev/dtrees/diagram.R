install.packages("htmlwidgets")
require(devtools)
install_github('rich-iannone/DiagrammeR')
require(DiagrammeR)


library(shiny)

ui = shinyUI(fluidPage(
  textInput('spec', 'Diagram Spec', value = ""),
  DiagrammeROutput('diagram')
))

server = function(input, output){
  output$diagram <- renderDiagrammeR(DiagrammeR(
    input$spec
  ))
}

shinyApp( ui = ui, server = server )


# paste

graph LR;
A-->B;
A-->C;
C-->E;
B-->D;
C-->D;
D-->F;
E-->F;
