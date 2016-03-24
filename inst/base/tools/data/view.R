#############################################
# View table output of the selected dataset
#############################################
output$ui_view_vars <- renderUI({
  vars <- varnames()
  if (not_available(vars)) return()
  isolate({
    if (not_available(r_state$view_vars)) {
      r_state$view_vars <<- NULL
      r_state$dataviewer_state <<- list()
      r_state$dataviewer_search_columns <<- NULL
    }
  })

  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_multiple("view_vars", vars, vars), multiple = TRUE,
    selectize = FALSE, size = min(15, length(vars)))
})

## see if you can figure out how to reset the indices for sorting and
## filtering variables as variable selection changes
output$ui_View <- renderUI({
  tagList(
    wellPanel(
      checkboxInput("view_pause", "Pause view", state_init("view_pause", FALSE)),
      uiOutput("ui_view_vars"),
      tags$table(
        tags$td(textInput("view_dat", "Store filtered data as:", paste0(input$dataset,"_view"))),
        tags$td(actionButton("view_store", "Store"), style="padding-top:30px;")
      )
    ),
    help_modal('View','view_help',inclMD(file.path(r_path,"base/tools/help/view.md")) %>% gsub("`","",.))
  )
})

my_dataTablesFilter = function(data, req) {
  ## to implement
}

observeEvent(input$dataviewer_search_columns, {
  r_state$dataviewer_search_columns <<- input$dataviewer_search_columns
})

observeEvent(input$dataviewer_state, {
  r_state$dataviewer_state <<-
    if (is.null(input$dataviewer_state)) list() else input$dataviewer_state
})

output$dataviewer <- DT::renderDataTable({
  if (not_available(input$view_vars)) return(data.frame())
  req(input$view_pause == FALSE, cancelOutput = TRUE)

  dat <- select_(.getdata(), .dots = input$view_vars)
  ## this causes problems when r_state is NULL?
  if (!identical(r_state$view_vars, input$view_vars)) {
    r_state$view_vars <<- input$view_vars
    r_state$dataviewer_state <<- list()
    r_state$dataviewer_search_columns <<- rep("", ncol(dat))
  }

  ## seems needed due to partial name matching on dataviewer_search
  search <- r_state$dataviewer_state$search$search
  if (is.null(search)) search <- ""

  if (nrow(dat) > 5000000) {
    fbox <- "none"
  } else {
    fbox <- list(position = "top")
  #   dc <- getclass(dat)
  #   if ("factor" %in% dc) {
  #     toChar <- sapply(select(dat, which(dc == "factor")), function(x) length(levels(x))) > 100
  #     if (any(toChar))
  #       dat <- mutate_each_(dat, funs(as.character), vars = names(toChar)[toChar])
  #   }
  }

  withProgress(message = 'Generating view table', value = 0,
    DT::datatable(dat, filter = fbox, selection = "none",
      rownames = FALSE, style = "bootstrap", escape = FALSE,
      options = list(
        stateSave = TRUE,   ## maintains state but does not show column filter settings
        searchCols = lapply(r_state$dataviewer_search_columns, function(x) list(search = x)),
        search = list(search = search, regex = TRUE),
        order = {if (is.null(r_state$dataviewer_state$order)) list()
                 else r_state$dataviewer_state$order},
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        processing = FALSE,
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
      ),
      callback = DT::JS("$(window).unload(function() { table.state.clear(); })")
    )
  )
})

observeEvent(input$view_store, {
  data_filter <- if (input$show_filter) input$data_filter else ""
  view_store(input$dataset, input$view_vars, input$view_dat, data_filter, input$dataviewer_rows_all)
  updateTextInput(session, "data_filter", value = "")
  updateCheckboxInput(session = session, inputId = "show_filter", value = FALSE)
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
    data_filter <- if (input$show_filter) input$data_filter else ""
    getdata(input$dataset, vars = input$view_vars, filt = data_filter,
            rows = input$dataviewer_rows_all, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)
