#############################################
# View table output of the selected dataset
#############################################
output$ui_view_vars <- renderUI({
  vars <- varnames()
  if (not_available(vars)) return()
  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_multiple("view_vars", vars, vars), multiple = TRUE,
    selectize = FALSE, size = min(15, length(vars)))
})

output$ui_View <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_view_vars"),
      tags$table(
        tags$td(textInput("view_dat", "Store filtered data as:", paste0(input$dataset,"_view"))),
        tags$td(actionButton("view_store", "Store"), style="padding-top:30px;")
      )
    ),
    help_modal('View','view_help',inclMD(file.path(r_path,"base/tools/help/view.md")))
  )
})

my_dataTablesFilter = function(data, req) {
  ## to implement
}

observeEvent(input$dataviewer_search_columns, {
  isolate({
    r_state$dataviewer_search_columns <<- input$dataviewer_search_columns
  })
})

observeEvent(input$dataviewer_state, {
  isolate({
    r_state$dataviewer_state <<- input$dataviewer_state
  })
})

output$dataviewer <- DT::renderDataTable({

  if (not_available(input$view_vars)) return()
  dat <- select_(.getdata(), .dots = input$view_vars)

  ## seems needed due to partial name matching on dataviewer_search
  search <- r_state$dataviewer_state$search$search
  if (is.null(search)) search <- ""

  if (nrow(dat) > 100000)  filt <- 'none'
  else filt <- list(position = "top")
  # else filt <- list(position = "top", clear = FALSE, plain = TRUE)
  # action = DT::dataTableAjax(session, dat, rownames = FALSE, filter = my_dataTablesFilter)
  DT::datatable(dat, filter = filt,
    rownames = FALSE, style = "bootstrap", escape = FALSE,
    options = list(

      stateSave = TRUE,   ## maintains state but does not show column filter settings
      searchCols = lapply(r_state$dataviewer_search_columns, function(x) list(search = x)),
      # search = list(regex = TRUE),
      search = list(search = search),
      order = r_state$dataviewer_state$order,

      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      processing = FALSE,
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
    ),
    callback = DT::JS("$('a#refresh_radiant').on('click', function() { table.state.clear(); });
                   $('input#uploadState').on('click', function() { table.state.clear(); });")
  )
})

min


observeEvent(input$view_store, {
  isolate({
    view_store(input$dataset, input$view_vars, input$view_dat, input$data_filter, input$dataviewer_rows_all)
    updateTextInput(session, "data_filter", value = "")
    updateCheckboxInput(session = session, inputId = "show_filter", value = FALSE)

  })
})

view_store <- function(dataset,
                       vars = "",
                       view_dat = dataset,
                       data_filter = "",
                       rows = NULL) {


  mess <-
    if (data_filter != "" && !is.null(rows))
      paste0("\nSaved filtered data: ", data_filter, " and view-filter (", lubridate::now(), ")")
    else if (is.null(rows))
      paste0("\nSaved filtered data: ", data_filter, " (", lubridate::now(), ")")
    else if (data_filter == "")
      paste0("\nSaved data with view-filter (", lubridate::now(), ")")
    else
      ""

  getdata(dataset, vars = vars, filt = data_filter, rows = rows, na.rm = FALSE) %>%
    save2env(dataset, view_dat, mess)

  updateSelectInput(session = session, inputId = "dataset", selected = view_dat)
  updateSelectInput(session = session, inputId = "view_vars", selected = vars)
}

output$dl_view_tab <- downloadHandler(
  filename = function() { paste0("view_tab.csv") },
  content = function(file) {
    getdata(input$dataset, vars = input$view_vars, filt = input$data_filter,
            rows = input$dataviewer_rows_all, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)

## cannot (re)set state ... yet
# search = list(search = 'Ma'), order = list(list(2, 'asc'), list(1, 'desc'))
# output$tbl_state <- renderPrint(str(input$dataviewer_state))
