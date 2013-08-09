N = 30; B0 = runif(1, -5, 5); B1 = runif(1, -5, 5); x = seq(-3, 3, length = N)
df = data.frame(x = x, y = B0 + B1*x + rnorm(N))

shinyServer(function(input, output) {
  
  output$plot = renderPlot({

    plot(y ~ x, data = df)
    abline(input$b0, input$b1, col = 'red')

    if (input$showFit) {
      fit = lm(y ~ x, data = df)
      abline(fit, col = 'green')
    }

  }, width = 700, height = 500)
  
})
