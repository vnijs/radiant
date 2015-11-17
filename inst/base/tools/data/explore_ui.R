#######################################
## Explore datasets
#######################################

default_funs <- c("length", "n_missing", "n_distinct", "mean_rm", "sd_rm", "min_rm", "max_rm")
expl_args <- as.list(formals(explore))

## list of function inputs selected by user
expl_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  expl_args$data_filter <- if (input$show_filter) input$data_filter else ""
  expl_args$dataset <- input$dataset
  for (i in r_drop(names(expl_args)))
    expl_args[[i]] <- input[[paste0("expl_",i)]]

  expl_args
})

## UI-elements for explore
output$ui_expl_vars <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- varnames()[isNum]
  if (not_available(vars)) return()


  selectInput("expl_vars", label = "Select variable(s):", choices = vars,
    selected = state_multiple("expl_vars",vars), multiple = TRUE,
    size = min(8, length(vars)), selectize = FALSE)
})

output$ui_expl_byvar <- renderUI({
  vars <- groupable_vars()
  if (not_available(vars)) return()

  if (any(vars %in% input$expl_vars)) {
    vars <- setdiff(vars, input$expl_vars)
  }

  isolate({
    if (available(r_state$expl_byvar) && all(r_state$expl_byvar %in% vars)) {
      ## can't use unique here - removes variable type information
      vars <- c(r_state$expl_byvar, vars) %>% .[!duplicated(.)]
    }

    sel <- use_input("expl_byvar", vars, fun = "state_multiple")
  })

  selectizeInput("expl_byvar", label = "Group by:", choices = vars,
    selected = sel, multiple = TRUE,
    options = list(placeholder = 'Select group-by variable',
                   plugins = list('remove_button', 'drag_drop'))
  )
})

output$ui_expl_fun <- renderUI({
  isolate({
    sel <- if (is_empty(input$expl_fun))  state_multiple("expl_fun", r_functions, default_funs)
           else input$expl_fun
  })
  selectizeInput("expl_fun", label = "Apply function(s):",
                 choices = r_functions, selected = sel, multiple = TRUE,
                 options = list(placeholder = 'Select functions',
                                plugins = list('remove_button', 'drag_drop'))
    )
})

output$ui_expl_top  <- renderUI({
  if (is_empty(input$expl_vars)) return()
  top_var = c("Function" = "fun", "Variables" = "var", "Group by" = "byvar")
  if (is_empty(input$expl_byvar)) top_var <- top_var[1:2]
  selectizeInput("expl_top", label = "Column variable:",
                 choices = top_var,
                 selected = state_single("expl_top", top_var, top_var[1]),
                 multiple = FALSE)
})

output$ui_expl_viz <- renderUI({
  checkboxInput('expl_viz', 'Show plot', value = state_init("expl_viz", FALSE))
})

output$ui_Explore <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_expl_vars"),
      uiOutput("ui_expl_byvar"),
      uiOutput("ui_expl_fun"),
      uiOutput("ui_expl_top"),
      with(tags, table(
        tr(
          td(textInput("expl_dat", "Store filtered data as:", "explore_dat")),
          td(actionButton("expl_store", "Store"), style="padding-top:30px;")
        )
      ))
    ),
    help_and_report(modal_title = "Explore",
                    fun_name = "explore",
                    help_file = inclMD(file.path(r_path,"base/tools/help/explore.md")))
  )
})

.explore <- reactive({
  if (not_available(input$expl_vars) || is.null(input$expl_top)) return()

  if (available(input$expl_byvar) && any(input$expl_byvar %in% input$expl_vars)) return()

  withProgress(message = 'Calculating', value = 0, {
    sshhr( do.call(explore, expl_inputs()) )
  })
})

observeEvent(input$explorer_search_columns, {
  isolate({
    r_state$explorer_search_columns <<- input$explorer_search_columns
  })
})

observeEvent(input$explorer_state, {
  isolate({
    r_state$explorer_state <<- input$explorer_state
  })
})

expl_reset <- function(var, ncol) {
  if (!identical(r_state[[var]], input[[var]])) {
    r_state[[var]] <<- input[[var]]
    r_state$explorer_state <<- list()
    r_state$explorer_search_columns <<- rep("", ncol)
  }
}

output$explorer <- DT::renderDataTable({
  expl <- .explore()
  if (is.null(expl)) return()
  expl$shiny <- TRUE

  ## resetting DT when changes occur
  nc <- ncol(expl$tab)
  expl_reset("expl_vars", nc)
  expl_reset("expl_byvar", nc)
  expl_reset("expl_fun", nc)
  if (!is.null(r_state$expl_top) && !is.null(input$expl_top) &&
      !identical(r_state$expl_top, input$expl_top)) {
    r_state$expl_top <<- input$expl_top
    r_state$explorer_state <<- list()
    r_state$explorer_search_columns <<- rep("", nc)
  }

  isolate({
    search <- r_state$explorer_state$search$search
    if (is.null(search)) search <- ""
    searchCols <- lapply(r_state$explorer_search_columns, function(x) list(search = x))
    order <- r_state$explorer_state$order
  })

  top <- ifelse (input$expl_top == "", "fun", input$expl_top)
  make_expl(expl, top = top, search = search,
            searchCols = searchCols, order = order)
})

output$dl_explore_tab <- downloadHandler(
  filename = function() { paste0("explore_tab.csv") },
  content = function(file) {
    dat <- .explore()
    if (is.null(dat)) {
      write.csv(data_frame("Data" = "[Empty]"),file, row.names = FALSE)
    } else {
      rows <- input$explorer_rows_all
      flip(dat, input$expl_top) %>%
        {if (is.null(rows)) . else slice(., rows)} %>%
        write.csv(file, row.names = FALSE)
    }
  }
)

observeEvent(input$expl_store, {
  isolate({
    dat <- .explore()
    if (is.null(dat)) return()
    rows <- input$explorer_rows_all
    name <- input$expl_dat
    tab <- dat$tab
    if (!is.null(rows) && !all(rows == 1:nrow(tab))) {
      tab <- tab %>% slice(., rows)
      for (i in c(dat$byvar,"variable"))
        tab[[i]] %<>% factor(., levels = unique(.))
    }

    env <- if (exists("r_env")) r_env else pryr::where("r_data")
    env$r_data[[name]] <- tab
    cat(paste0("Dataset r_data$", name, " created in ", environmentName(env), " environment\n"))
    env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique

    updateSelectInput(session, "dataset", selected = name)
  })
})

output$expl_summary <- renderPrint({
  if (not_available(input$expl_vars)) return(invisible())
    withProgress(message = 'Calculating', value = 0, {
      .explore() %>% { if (is.null(.)) invisible() else summary(., top = input$expl_top) }
    })
})

observeEvent(input$explore_report, {
  isolate({
    ## add command to store data and/or download it
    # xcmd <-
    #     paste0("# store_reg(result, data = '", input$dataset, "', type = 'prediction', name = '", input$reg_store_pred_name,"')\n") %>%
    update_report(inp_main = c(clean_args(expl_inputs(), expl_args), tabsort = "", tabfilt = ""),
                  fun_name = "explore",
                  inp_out = list(list(top = input$expl_top)),
                  outputs = c("summary"),
                  figs = FALSE)
  })
})
