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
    help_modal('View','view_help',inclMD(file.path(r_path,"base/tools/help/view.md")))
  )
})

my_dataTablesFilter = function(data, req) {
  ## to implement
}

output$dataviewer <- DT::renderDataTable({

  if (not_available(input$view_vars)) return()
  dat <- select_(.getdata(), .dots = input$view_vars)

  # action = DT::dataTableAjax(session, dat, rownames = FALSE, filter = my_dataTablesFilter)
  DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
    rownames = FALSE, style = "bootstrap", escape = FALSE,
    options = list(
      # search = list(search = rnorm(1)), order = list(list(2, 'asc'), list(1, 'desc')),
      # stateSave = TRUE,   # maintains but does not show column filter settings
      search = list(regex = TRUE),
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      autoWidth = TRUE,
      processing = FALSE,
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
    )
  )
})

# search = list(search = 'Ma'), order = list(list(2, 'asc'), list(1, 'desc'))
# output$tbl_state = renderPrint(str(input$dataviewer_state))
