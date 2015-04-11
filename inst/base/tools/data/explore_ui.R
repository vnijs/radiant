#######################################
# Explore datasets
#######################################

# expl_args <- as.list(formals(explore))
expl_args <- list(dataset = "", expl_vars = "", data_filter = "",
                  expl_byvar = "", expl_fun = c("length", "mean_rm"))

# list of function inputs selected by user
expl_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(expl_args))
    expl_args[[i]] <- input[[i]]
  if(!input$show_filter) expl_args$data_filter = ""

  if(is_empty(input$expl_byvar)) expl_args$expl_fun <- c("length", "mean_rm")

  expl_args   # expl_args is only changed inside this function
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
    selected = state_multiple("expl_byvar",vars, ""), multiple = TRUE,
    options = list(placeholder = 'Select group-by variable',
                   plugins = list('remove_button', 'drag_drop'))
  )
})

output$ui_expl_fun <- renderUI({
  if(is_empty(input$expl_byvar)) return()
  selectizeInput("expl_fun", label = "Apply function(s):",
                 choices = expl_functions,
                 selected = state_multiple("expl_fun", expl_functions, c("length","mean_rm")),
                 multiple = TRUE,
                 options = list(placeholder = 'Select functions',
                                plugins = list('remove_button', 'drag_drop'))
    )
})

output$ui_expl_viz <- renderUI({
  if(is_empty(input$expl_byvar)) return()
  checkboxInput('expl_viz', 'Show plot',
                value = state_init("expl_viz", FALSE))
})

output$ui_Explore <- renderUI({
  list(
    wellPanel(
      uiOutput("ui_expl_vars"),
      uiOutput("ui_expl_byvar"),
      uiOutput("ui_expl_fun"),
      div(class="row",
        div(class="col-xs-6", checkboxInput('expl_tab', 'Show table',
            value = state_init("expl_tab", TRUE))),
        div(class="col-xs-6", uiOutput("ui_expl_viz"))
      )
    ),
    help_and_report(modal_title = "Explore",
                    fun_name = "explore",
                    help_file = inclMD(file.path(path,"/base/tools/help/explore.md")))
  )
})

.explore <- reactive({
  if(not_available(input$expl_vars)) return()
  withProgress(message = 'Calculating', value = 0, {
    do.call(explore, expl_inputs())
  })
})

output$expl_summary <- renderPrint({
  if(!is.null(input$expl_tab) && input$expl_tab)
    .explore() %>% { if(is.null(.)) invisible() else summary(.) }
})

expl_plot_width <- function() 650
expl_plot_height <- function()
  400 * length(input$expl_fun) * length(input$expl_vars)

output$expl_plots <- renderPlot({
  if(!input$expl_viz || is.null(input$expl_byvar)) return()
  withProgress(message = 'Making plot', value = 0, {
    .explore() %>% { if(is.null(.)) invisible() else print(plot(., shiny = TRUE)) }
  })
}, width = expl_plot_width, height = expl_plot_height)

observe({
  if(not_pressed(input$explore_report)) return()
  isolate({
    # if(!is.null(input$expl_viz) && input$expl_viz == TRUE) {
    if(!is_empty(input$expl_byvar) && input$expl_viz == TRUE) {
      inp_out <- list("","")
      outputs <- c("summary","plot")
      figs <- TRUE
    } else {
      outputs <- c("summary")
      inp_out <- list("","")
      figs <- FALSE
    }
    update_report(inp_main = clean_args(expl_inputs(), expl_args),
                  fun_name = "explore",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * expl_plot_width()/650,2),
                  fig.height = round(7 * expl_plot_height()/650,2))
  })
})

