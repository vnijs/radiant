#######################################
# Simulate data
#######################################

sim_args <- as.list(formals(simulater))

## list of function inputs selected by user
sim_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(sim_args))
    sim_args[[i]] <- input[[paste0("sim_",i)]]

  sim_args
})

sim_types <- list("Constant" = "const", "Normal" = "norm", "Uniform" = "unif",
                  "Discrete" = "discrete")

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

output$ui_Simulater <- renderUI({
  tagList(
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
      textInput("sim_name", "Sim name:",
                state_init("sim_name", "sim1")),
      actionButton("runSim", "Simulate")
    ),
    help_and_report(modal_title = "Simulate",
                    fun_name = "simulater",
                    help_file = inclMD(file.path(r_path,"base/tools/help/simulater.md")))
  )
})

.simulater <- eventReactive(input$runSim, {
  withProgress(message = 'Simulating', value = 0, {
    do.call(simulater, sim_inputs())
  })
})

output$sim_summary <- renderPrint({
  .simulater() %>% { if (is.null(.)) invisible() else summary(.) }
})

sim_plot_width <- function() 650

sim_plot_height <- function()
  .simulater() %>% { if (is.data.frame(.)) ceiling(ncol(.)/2) * 200 else 200 }

output$sim_plots <- renderPlot({
  withProgress(message = 'Making simulation plots', value = 0, {
    .simulater() %>% { if (is.null(.)) invisible() else print(plot(., shiny = TRUE)) }
  })
}, width = sim_plot_width, height = sim_plot_height)

observe({
  if (not_pressed(input$simulater_report)) return()
  isolate({
    update_report(inp_main = clean_args(sim_inputs(), sim_args),
                  fun_name = "simulater", inp_out = list("",""),
                  outputs = c("summary","plot"), figs = TRUE,
                  fig.width = round(7 * sim_plot_width()/650,2),
                  fig.height = round(7 * sim_plot_height()/400,2))
  })
})
