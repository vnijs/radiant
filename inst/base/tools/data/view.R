#############################################
# View table output of the selected dataset
#############################################
output$ui_view_vars <- renderUI({
  vars <- varnames()
  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_multiple("view_vars",vars, vars), multiple = TRUE,
    selectize = FALSE, size = min(15, length(vars)))
})

output$ui_View <- renderUI({
  list(
    wellPanel(
      uiOutput("ui_view_vars")
    ),
    help_modal('View','viewHelp',inclMD(paste0(path,"/base/tools/help/view.md")))
  )
})

# output$dataviewer <- renderDataTable({

#   if(not_available(input$view_vars)) return()
#   select_(.getdata(), .dots = input$view_vars)

# }, options = list(orderClasses = TRUE, caseInsensitive = TRUE,
#   lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
#   pageLength = 10, search = list(regex = TRUE)))


# use DT to add dplyr - server side code
output$dataviewer <- DT::renderDataTable({

  if(not_available(input$view_vars)) return()
  select_(.getdata(), .dots = input$view_vars) -> dat
  DT::datatable(dat, rownames = FALSE, filter = "bottom",
    # server = TRUE,
    options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All'))
    )
  )
})
