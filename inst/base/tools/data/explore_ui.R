# UI-elements for explore
output$uiExpl_columns <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- varnames()[isNum]
  selectInput("expl_columns", label = "Select columns(s):", choices = vars,
    selected = state_multiple("expl_columns",vars), multiple = TRUE,
    size = min(8, length(vars)), selectize = FALSE)
})

output$uiExpl_byvar <- renderUI({
  vars <- groupable_vars()
  selectizeInput("expl_byvar", label = "Group by:", choices = vars,
    selected = state_multiple("expl_byvar",vars), multiple = TRUE,
    options = list(placeholder = 'Select group-by variable',
                   plugins = list('remove_button', 'drag_drop'))
  )
})

expl_functions <- list("n" = "length", "mean" = ".mean", "median" = ".median",
                       "min" = ".min", "max" = ".max", "25%" = "p25",
                       "75%" = "p75", "sd" = ".sd", "se" = "serr",
                       "cv" = "cv", "skew" = "skew", "kurtosis" = "kurtosi",
                       "# missing" = "nmissing")

output$uiExpl_function <- renderUI({
  if(is.null(input$expl_byvar)) return()
  selectizeInput("expl_function", label = "Apply function(s):",
                 choices = expl_functions,
                 selected = state_multiple("expl_function",
                                           expl_functions, c("length","mean")),
                 multiple = TRUE,
                 options = list(placeholder = 'Select functions',
                                plugins = list('remove_button', 'drag_drop'))
    )
})

output$uiExpl_show_viz <- renderUI({
  if(is.null(input$expl_byvar)) return()
  checkboxInput('expl_show_viz', 'Show plot',
                value = state_init("expl_show_viz", FALSE))
})

output$ui_Explore <- renderUI({
  list(
    wellPanel(
      uiOutput("uiExpl_columns"),
      uiOutput("uiExpl_byvar"),
      uiOutput("uiExpl_function"),
      div(class="row",
        div(class="col-xs-6", checkboxInput('expl_show_tab', 'Show table',
            value = state_init("expl_show_tab", TRUE))),
        div(class="col-xs-6", uiOutput("uiExpl_show_viz"))
      )
    ),
    help_modal('Explore','exploreHelp',inclMD("../base/tools/help/explore.md"))
  )
})

.explore <- reactive({
  if(input$expl_columns %>% not_available) return()

  withProgress(message = 'Calculating', value = 0, {
    explore(input$dataset, input$expl_columns, input$expl_byvar,
            input$expl_function, input$expl_show_tab, input$expl_show_viz)
  })
})

.summary_explore <- reactive({

  result <- .explore()
  if(is.null(result)) return(invisible())
  summary_explore(result)
})

output$expl_summary <- renderPrint({
  if(!is.null(input$expl_show_tab) && !input$expl_show_tab)
    return(invisible())
  .summary_explore()
})

.plots_explore <- reactive({

  result <- .explore()
  if(is.null(result)) return()

  withProgress(message = 'Making plot', value = 0, {
    plots_explore(result)
  })
})

expl_plot_width <- function() 650
expl_plot_height <- function()
  400 * length(input$expl_function) * length(input$expl_columns)

output$expl_plots <- renderPlot({
  if(!input$expl_show_viz || is.null(input$expl_byvar)) return()
  .plots_explore() %>% print
}, width = expl_plot_width, height = expl_plot_height)
