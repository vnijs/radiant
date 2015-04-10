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

output$dataviewer <- DT::renderDataTable({

  if(not_available(input$view_vars)) return()

  dat <- select_(.getdata(), .dots = input$view_vars)
  action = DT::dataTableAjax(session, dat, rownames = FALSE)

  DT::datatable(dat, filter = "bottom", rownames = FALSE,
    server = TRUE,
    options = list(
      ajax = list(url = action),
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      processing = FALSE,
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
    )
  )
})
