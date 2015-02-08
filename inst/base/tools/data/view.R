#############################################
# View table output of the selected dataset
#############################################
output$uiView_vars <- renderUI({
  vars <- varnames()
  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_init_multvar("view_vars",vars, vars), multiple = TRUE,
    selectize = FALSE, size = length(vars))
})

output$ui_View <- renderUI({
  list(
    wellPanel(
      uiOutput("uiView_vars")
    ),
    helpModal('View','viewHelp',inclMD("../base/tools/help/view.md"))
  )
})

output$dataviewer <- renderDataTable({

  if(input$view_vars %>% not_available) return()
  select_(getdata(), .dots = input$view_vars)

}, options = list(orderClasses = TRUE, caseInsensitive = TRUE,
  lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
  pageLength = 10, search = list(regex = TRUE)))

