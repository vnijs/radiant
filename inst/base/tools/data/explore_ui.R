#######################################
# Explore datasets
#######################################

default_funs <- c("length", "nmissing", "mean_rm", "sd_rm", "min_rm", "max_rm")
# expl_format <- c("None" = "none", "Color bar" = "color_bar", "Heat map" = "heat")

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

# UI-elements for explore
output$ui_expl_vars <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- varnames()[isNum]
  selectInput("expl_vars", label = "Select variable(s):", choices = vars,
    selected = state_multiple("expl_vars",vars), multiple = TRUE,
    size = min(8, length(vars)), selectize = FALSE)
})

output$ui_expl_byvar <- renderUI({
  vars <- groupable_vars()
  selectizeInput("expl_byvar", label = "Group by:", choices = vars,
    selected = state_multiple("expl_byvar", vars, ""), multiple = TRUE,
    options = list(placeholder = 'Select group-by variable',
                   plugins = list('remove_button', 'drag_drop'))
  )
})

output$ui_expl_fun <- renderUI({
  isolate({
    sel <- if(is_empty(input$expl_fun))  state_multiple("expl_fun", r_functions, default_funs)
           else input$expl_fun
  })
  selectizeInput("expl_fun", label = "Apply function(s):",
                 choices = r_functions, selected = sel, multiple = TRUE,
                 options = list(placeholder = 'Select functions',
                                plugins = list('remove_button', 'drag_drop'))
    )
})

output$ui_expl_top  <- renderUI({
  top_var = c("Function" = "fun", "Variables" = "var", "Group by" = "byvar")
  if (is_empty(input$expl_byvar)) top_var <- top_var[1:2]
  selectizeInput("expl_top", label = "Column variable:",
                 choices = top_var,
                 selected = state_single("expl_top", top_var, top_var[1]),
                 multiple = FALSE)
})

# output$ui_expl_format  <- renderUI({
#   selectizeInput("expl_format", label = "Conditional formatting:",
#                  choices = expl_format,
#                  selected = state_single("expl_format", expl_format, "none"),
#                  multiple = FALSE)
# })

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
      # uiOutput("ui_expl_format")
    ),
    help_and_report(modal_title = "Explore",
                    fun_name = "explore",
                    help_file = inclMD(file.path(r_path,"base/tools/help/explore.md")))
  )
})

.explore <- reactive({
  if (not_available(input$expl_vars)) return()
  withProgress(message = 'Calculating', value = 0, {
    sshhr( do.call(explore, expl_inputs()) )
  })
})

output$explorer <- DT::renderDataTable({
  expl <- .explore()
  if (is.null(expl)) return()
  expl$shiny <- TRUE
  # make_expl(expl, top = input$expl_top, format = input$expl_format)
  make_expl(expl, top = input$expl_top)
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

    # updateTabsetPanel(session, "tabs_data", selected = "Visualize")
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
    update_report(inp_main = clean_args(expl_inputs(), expl_args),
                  fun_name = "explore",
                  inp_out = list(list(top = input$expl_top)),
                  outputs = c("summary"),
                  figs = FALSE)
  })
})
