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
      uiOutput("ui_view_vars"),
      textInput("dt_dat", "Save filtered data to:", input$dataset),
      actionButton("view_save", "Save")
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
      # stateSave = TRUE,   ## maintains state but does not show column filter settings
      # search = list(regex = TRUE, search = "G", order = list(list(2, 'asc'), list(1, 'desc'))),
      search = list(regex = TRUE),
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      processing = FALSE,
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
    )
  )
})

observeEvent(input$view_save, {
  isolate({
    save_view(input$dataset, input$dt_dat, input$data_filter, input$dataviewer_rows_all)
    updateTextInput(session, "data_filter", value = "")
    updateCheckboxInput(session = session, inputId = "show_filter", value = FALSE)

  })
})

save_view <- function(dataset,
                      dt_dat = dataset,
                      data_filter = "",
                      dt_rows = NULL) {

  mess <-
    if (data_filter != "" && !is.null(dt_rows))
      paste0("\nSaved filtered data: ", data_filter, " and view-filter (", lubridate::now(), ")")
    else if (is.null(dt_rows))
      paste0("\nSaved filtered data: ", data_filter, " (", lubridate::now(), ")")
    else if (data_filter == "")
      paste0("\nSaved data with view-filter (", lubridate::now(), ")")
    else
      ""

  getdata(dataset, filt = data_filter, na.rm = FALSE) %>%
    {if (is.null(dt_rows)) . else slice(., dt_rows)} %>%
    save2env(dataset, dt_dat, mess)
}

# output$dt_rows <- renderPrint({
#   input$dataviewer_rows_all
# })

# data_filter <- "clarity == 'I1' ||| dataviewer_rows_all"
# data_filter <- "||| dataviewer_rows_all"
# data_filter <- ""
# strsplit(data_filter, "\\|\\|\\|")

# search = list(search = 'Ma'), order = list(list(2, 'asc'), list(1, 'desc'))
# output$tbl_state <- renderPrint(str(input$dataviewer_state))
