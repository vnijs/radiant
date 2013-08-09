shinyUI(
  pageWithSidebar(
  
    headerPanel('Regression applet'),
    
    sidebarPanel(

      numericInput('b0', 'Intercept', min = -5, max = 5, value = 0, step = .1),
      numericInput('b1', 'Slope', min = -5, max = 5, value = 0.1, step = .1),
      checkboxInput('showFit', 'Show OLS fit'),
      helpText('Adapted from appstat by Yihui Xie')

    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )
)
