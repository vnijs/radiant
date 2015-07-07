#######################################
## Simulate data
#######################################

sim_types <- list("Constant" = "const", "Normal" = "norm", "Uniform" = "unif",
                  "Discrete" = "discrete")

sim_args <- as.list(formals(simulater))

## list of function inputs selected by user
sim_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(sim_args))
    sim_args[[i]] <- input[[paste0("sim_",i)]]

  for (i in sim_types)
    if (!i %in% input$sim_types) sim_args[[i]] <- ""

  sim_args
})

rep_args <- as.list(formals(repeater))

## list of function inputs selected by user
rep_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(rep_args))
    rep_args[[i]] <- input[[paste0("rep_",i)]]

  # rep_args[[sim_ret]] <- sim_inputs()
  rep_args[["sim"]] <- .simulater()

  rep_args
})

rep_sum_args <- as.list(if (exists("summary.repeater")) formals(summary.repeater)
                        else formals(radiant:::summary.repeater))

## list of function inputs selected by user
rep_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(rep_sum_args))
    rep_sum_args[[i]] <- input[[paste0("rep_",i)]]
  rep_sum_args
})

rep_plot_args <- as.list(if (exists("plot.repeater")) formals(plot.repeater)
                         else formals(radiant:::plot.repeater))

## list of function inputs selected by user
rep_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(rep_plot_args))
    rep_plot_args[[i]] <- input[[paste0("rep_",i)]]
  rep_plot_args
})




textinput_maker <- function(id = "const", lab = "Constant") {

  lab <- paste0(lab, if(id == "form") ":" else " variables:")
  id <- paste0("sim_", id)

  tagList(
    tags$label(lab, `for` = id), br(),
    tags$textarea(state_init(id), id = id, type = "text", rows="2",
                  class = "form-control")
  )
}

output$ui_sim_types <- renderUI({
  selectizeInput("sim_types", label = "Select types:",
    choices = sim_types, multiple = TRUE,
    selected = state_multiple("sim_types", sim_types, "const"),
    options = list(placeholder = 'Select types',
                   plugins = list('remove_button', 'drag_drop'))
    )
})

sim_vars <- reactive({
  .simulater() %>% { if (is.null(.)) character(0) else colnames(.$dat) }
})

output$ui_rep_vars <- renderUI({
  # vars <- .simulater() %>% { if (is.null(.)) character(0) else colnames(.$dat) }
  vars <- sim_vars()
  # if(is.null(vars)) return()
  selectizeInput("rep_vars", label = "Select variables:",
    choices = vars, multiple = TRUE,
    selected = state_multiple("rep_vars", vars),
    options = list(placeholder = 'Select variables',
                   plugins = list('remove_button', 'drag_drop'))
    )
})

output$ui_rep_sum_vars <- renderUI({
  vars <- sim_vars()
  selectizeInput("rep_sum_vars", label = "Output variables:",
    choices = vars, multiple = TRUE,
    selected = state_multiple("rep_sum_vars", vars),
    options = list(placeholder = 'Select variables',
                   plugins = list('remove_button', 'drag_drop'))
    )
})

output$ui_rep_byvar <- renderUI({
  # vars <- c("sim","run",sim_vars())
  vars <- c("sim","run")
  selectizeInput("rep_byvar", label = "Group by:", choices = vars,
    selected = state_single("rep_byvar",vars, ""), multiple = FALSE,
    options = list(placeholder = 'Select group-by variable')
    #                plugins = list('remove_button', 'drag_drop'))
  )
})

output$ui_rep_fun <- renderUI({
  sel <- if(is_empty(input$rep_fun)) state_multiple("rep_fun", expl_functions, c("sum_rm","mean_rm"))
         else input$rep_fun

  selectizeInput("rep_fun", label = "Apply function(s):",
                 choices = expl_functions,
                 selected = sel,
                 multiple = TRUE,
                 options = list(placeholder = 'Select functions',
                                plugins = list('remove_button', 'drag_drop'))
    )
})

output$ui_simulater <- renderUI({
  tagList(
    conditionalPanel(condition = "input.tabs_simulate == 'Model'",
      wellPanel(
        uiOutput("ui_sim_types"),
        conditionalPanel(condition = "input.sim_types.indexOf('const') >= 0",
          textinput_maker("const","Constant")
        ),
        conditionalPanel(condition = "input.sim_types.indexOf('unif') >= 0",
          textinput_maker("unif","Uniform")
        ),
        conditionalPanel(condition = "input.sim_types.indexOf('norm') >= 0",
          textinput_maker("norm","Normal")
        ),
        conditionalPanel(condition = "input.sim_types.indexOf('discrete') >= 0",
          textinput_maker("discrete","Discrete")
        ),
        textinput_maker("form","Formula"),

        with(tags, table(
          td(textInput("sim_seed", "Set random seed:",
                       value = state_init('sim_seed', 1234))),
          td(numericInput("sim_nr", "# runs:", min = 1, max = 10^6,
                          value = state_init('sim_nr', 1000)))
          )
        ),
        textInput("sim_name", "Sim name:", state_init("sim_name", "sim1")),
        actionButton("runSim", "Simulate")
      ),
      help_and_report(modal_title = "Simulate", fun_name = "simulater",
        help_file = inclMD(file.path(r_path,"quant/tools/help/simulater.md")))
    ),
    conditionalPanel(condition = "input.tabs_simulate == 'Repeat'",
      wellPanel(
        uiOutput("ui_rep_vars"),
        uiOutput("ui_rep_sum_vars"),
        uiOutput("ui_rep_byvar"),
        uiOutput("ui_rep_fun"),
        with(tags, table(
          td(textInput("rep_seed", "Set random seed:",
                       value = state_init('rep_seed', 1234))),
          td(numericInput("rep_nr", "# runs:", min = 1, max = 10^6,
                          value = state_init('rep_nr', 12)))
          )
        ),
        actionButton("runRepeat", "Simulate")
      ) #,
      # help_and_report(modal_title = "Repeat simulation", fun_name = "rsimulater",
      #   help_file = inclMD(file.path(r_path,"quant/tools/help/rsimulater.md")))
    )
  )
})

## output is called from the main radiant ui.R
output$simulater <- renderUI({

    register_print_output("summary_simulate", ".summary_simulate")
    register_plot_output("plot_simulate", ".plot_simulate",
                          width_fun = "sim_plot_width",
                          height_fun = "sim_plot_height")

    register_print_output("summary_repeat", ".summary_repeat")
    register_plot_output("plot_repeat", ".plot_repeat",
                          width_fun = "rep_plot_width",
                          height_fun = "rep_plot_height")

    ## mulitple tabs with components stacked
    sim_output_panels <- tabsetPanel(
      id = "tabs_simulate",
      tabPanel("Model",
        verbatimTextOutput("summary_simulate"),
        plotOutput("plot_simulate", height = "100%")
      ),
      tabPanel("Repeat",
        verbatimTextOutput("summary_repeat"),
        plotOutput("plot_repeat", height = "100%")
      )
    )

    stat_tab_panel(menu = "Decide",
                  tool = "Simulate",
                  data = NULL,
                  tool_ui = "ui_simulater",
                  output_panels = sim_output_panels)
})

.simulater <- eventReactive(input$runSim, {
  withProgress(message = 'Simulating', value = 0, {
    do.call(simulater, sim_inputs())
  })
})

.summary_simulate <- reactive({
  .simulater() %>% { if (is.null(.)) invisible() else summary(.) }
})

sim_plot_width <- function() 650
sim_plot_height <- function()
  .simulater() %>% { if (is.list(.)) ceiling(ncol(.$dat)/2) * 200 else 200 }

.plot_simulate <- reactive({
  .simulater() %>% { if (is.null(.)) invisible() else plot(., shiny = TRUE) }
})

.repeater <- eventReactive(input$runRepeat, {
  withProgress(message = 'Replicating simulation', value = 0, {
    do.call(repeater, rep_inputs())
  })
})

.summary_repeat <- reactive({
  object <- .repeater()
  if (is.null(object)) return(invisible())

  do.call(summary, c(list(object = object), rep_sum_inputs()))
})

rep_plot_width <- function() 650
rep_plot_height <- function() {
  nrPlots <- length(input$rep_fun) * length(input$rep_sum_vars)
  if (nrPlots == 0) 200 else ceiling(nrPlots/2) * 200
}

.plot_repeat <- reactive({

  object <- .repeater()
  if (is.null(object)) return(invisible())

  # do.call(summary, c(list(object = object), rep_sum_inputs()))

  rep_plot_inputs() %>% { .$shiny <- TRUE; . } %>%
    { do.call(plot, c(list(x = object), .)) }
  # .repeater() %>% { if (is.null(.)) invisible() else plot(., shiny = TRUE) }
})

report_cleaner <- function(x) x %>% gsub("\n",";",.) %>% gsub("[;]{2,}",";",.)

# observe({
# if (not_pressed(input$simulater_report)) return()
observeEvent(input$simulater_report, {
  isolate({
    update_report(inp_main = clean_args(sim_inputs(), sim_args) %>% lapply(report_cleaner),
                  fun_name = "simulater", inp_out = list("",""),
                  outputs = c("summary","plot"), figs = TRUE,
                  fig.width = round(7 * sim_plot_width()/650,2),
                  fig.height = round(7 * sim_plot_height()/400,2))
  })
})

