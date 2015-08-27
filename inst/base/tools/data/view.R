# observeEvent(input$dataset, {
#   ## reset r_state for DT tables when dataset is changed
#   isolate({
#     if (is.null(input$dataset)) return()
#     if (is.null(r_state$dataset)) r_state$dataset <<- input$dataset
#     if (r_state$dataset == input$dataset) return()

#     # if (r_state$dataset == input$dataset)
#     #   return()
#     # else
#     #   r_state$dataset<<- input$dataset

#     # r_state$dataviewer_state <<- list()
#     # r_state$dataviewer_search_columns <<- NULL
#     # r_state$pivotr_state <<- list()
#     # r_state$pivotr_search_columns <<- NULL
#     # r_state$explorer_state <<- list()
#     # r_state$explorer_search_columns <<- NULL
#   })
# })

#############################################
# View table output of the selected dataset
#############################################
output$ui_view_vars <- renderUI({
  vars <- varnames()
  if (not_available(vars)) return()
  isolate({
    if (not_available(r_state$view_vars)) {
      r_state$view_vars <<- NULL
      # r_state$view_vars <<- input$view_vars
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
observeEvent(input$view_vars, {

  # r_state$view_vars <<- input$view_vars

  # print(input$view_vars)
  # print(input$dataviewer_state$order %>% unlist)
  # print(r_state$dataviewer_state$order %>% unlist)
  # r_state$dataviewer_state$order <<- list()

  # r_state <- list()
  # r_state$dataviewer_state$order
  # r_state$dataviewer_state <<- list()
  # r_state$dataviewer_search_columns <<- rep("", length(input$view_vars))
  # r_state$dataviewer_search_columns <<- rep("", length(input$view_vars))
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

    # +++++++++++++++++++++++++
    # NEED MATCH THE INDEX TO THE NAME FOR COLUM SEARCHES - ALSO APPLIES TO
    # PIVOT AND EXPLORE
    # +++++++++++++++++++++++++

    # print(input$dataviewer_search_columns)
    # print(r_state$dataviewer_search_columns)

    r_state$dataviewer_search_columns <<- input$dataviewer_search_columns
  })
})

observeEvent(input$dataviewer_state, {
# observeEvent(input$dataviewer_state$order, {
  isolate({

    # print(input$dataviewer_state$order %>% unlist)
    # print(r_state$dataviewer_state$order %>% unlist)

    r_state$dataviewer_state <<-
      if (is.null(input$dataviewer_state)) list() else input$dataviewer_state

    # r_state$dataviewer_state$order <<-
    #   if (is.null(input$dataviewer_state$order)) list() else input$dataviewer_state$order

    # print(r_state$dataviewer_state$order %>% unlist)
    # print("===")
  })
})

output$dataviewer <- DT::renderDataTable({

  if (not_available(input$view_vars)) return()

  dat <- select_(.getdata(), .dots = input$view_vars)

  ## resetting if needed
  # print(r_state$view_vars)
  # print(input$view_vars)

  ## this causes problems when r_state is NULL to start
  # if (!is.null(r_state$view_vars) && !identical(r_state$view_vars, input$view_vars)) {

  ## this causes problems when r_state is NULL to latter on ??
  if (!identical(r_state$view_vars, input$view_vars)) {
    # print(r_state$view_vars)
    # print("reset")
    r_state$view_vars <<- input$view_vars
    r_state$dataviewer_state <<- list()
    r_state$dataviewer_search_columns <<- rep("", ncol(dat))
  }

  ## seems needed due to partial name matching on dataviewer_search
  search <- r_state$dataviewer_state$search$search
  if (is.null(search)) search <- ""
  # search <- ""

  if (nrow(dat) > 100000)  filt <- 'none'
  else filt <- list(position = "top")
  # else filt <- list(position = "top", clear = FALSE, plain = TRUE)
  # action = DT::dataTableAjax(session, dat, rownames = FALSE, filter = my_dataTablesFilter)
  DT::datatable(dat, filter = filt,
    rownames = FALSE, style = "bootstrap", escape = FALSE,
    options = list(

      stateSave = TRUE,   ## maintains state but does not show column filter settings

      searchCols = lapply(r_state$dataviewer_search_columns, function(x) list(search = x)),
      # searchCols = lapply(rep("", ncol(dat)), function(x) list(search = x)),
      # search = list(regex = TRUE),
      search = list(search = search),
      # search = list(search = ""),
      order = {if (is.null(r_state$dataviewer_state$order)) list()
              else r_state$dataviewer_state$order},
      # order = list(),
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      processing = FALSE,
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
    ),
    callback = DT::JS("$(window).unload(function() { table.state.clear(); })")
    # callback = DT::JS("$('a#refresh_radiant').on('click', function() { table.state.clear(); });
                   # $('input#uploadState').on('click', function() { table.state.clear(); });")
                   # $('select#dataset').onchange(function() { table.state.clear(); });")
  )

# })
})


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
