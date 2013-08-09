################################################################
# mytool
################################################################

N = 30; B0 = runif(1, -5, 5); B1 = runif(1, -5, 5); x = seq(-3, 3, length = N)
df = data.frame(x = x, y = B0 + B1*x + rnorm(N))

ui_mytool <- function() {
  wellPanel(
      numericInput('b0', 'Intercept', min = -5, max = 5, value = 0, step = .1),
      numericInput('b1', 'Slope', min = -5, max = 5, value = 0.1, step = .1),
      checkboxInput('showFit', 'Show OLS fit'),
      helpText('Adapted from appstat by Yihui Xie')
  )
}

summary.mytool <- function(result) summary(result)

plot.mytool <- function(result) {

  plot(y ~ x, data = df)
  abline(input$b0, input$b1, col = 'red')

  if (input$showFit) abline(result, col = 'green')
}

mytool <- reactive({ lm(y ~ x, data = df) })
