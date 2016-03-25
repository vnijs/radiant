###############################
# Sample size
###############################
ssc_type <- c("Mean" = "mean", "Proportion" = "proportion")
ssc_alternative <- c("Two sided" = "two.sided", "One sided" = "one.sided")

## list of function arguments
ssc_args <- as.list(formals(sample_size_comp))

## list of function inputs selected by user
ssc_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ssc_args))
    ssc_args[[i]] <- input[[paste0("ssc_",i)]]
  ssc_args
})

output$ui_sample_size_comp <- renderUI({
  tagList(
    wellPanel(
      radioButtons(inputId = "ssc_type", label = NULL, choices = ssc_type,
        selected = state_init("ssc_type", "mean"), inline = TRUE),
      numericInput("ssc_n", "Sample size (n):", min = 1,
        value = state_init("ssc_n", NA), step = 1),
      conditionalPanel(condition = "input.ssc_type == 'mean'",
        numericInput("ssc_delta", "Delta:",
          value = state_init("ssc_delta", 2), step = 1),
        numericInput("ssc_sd", "Std. deviation (sd):", min = 0,
          value = state_init("ssc_sd", 10), step = 1)
      ),
      conditionalPanel(condition = "input.ssc_type != 'mean'",
        numericInput("ssc_p1", "Proportion 1 (p1):", min = 0,
          max = 1, value = state_init("ssc_p1", .1), step = .05),
        numericInput("ssc_p2", "Proportion 2 (p2):", min = 0, max = 1,
          value = state_init("ssc_p2", .15), step = .05)
      ),
      numericInput("ssc_conf_lev", "Confidence level:", min = 0, max = 1,
        value = state_init("ssc_conf_lev", 0.95), step = .05),
      numericInput("ssc_power", "Power:", min = 0, max = 1,
        value = state_init("ssc_power", 0.8), step = .05),
      numericInput("ssc_ratio", "Ratio (n1 / n2):", min = 0,
        value = state_init("ssc_ratio", 1), step = .05),
      radioButtons(inputId = "ssc_alternative", label = NULL, choices = ssc_alternative,
        selected = state_init("ssc_alternative", "two.sided"), inline = TRUE)
    ),
    help_and_report(modal_title = 'Sample size (compare)', fun_name = 'sample_size_comp',
                    help_file = inclRmd(file.path(r_path,"quant/tools/help/sample_size_comp.Rmd")))
  )
})

output$sample_size_comp <- renderUI({

    register_print_output("summary_sample_size_comp", ".summary_sample_size_comp")

    ## one output with components stacked
    ssc_output_panels <- tagList(
       tabPanel("Summary", verbatimTextOutput("summary_sample_size_comp"))
    )

    stat_tab_panel(menu = "Sample",
                  tool = "Sample size (compare)",
                  data = NULL,
                  tool_ui = "ui_sample_size_comp",
                  output_panels = ssc_output_panels)
})

.sample_size_comp <- reactive({
  do.call(sample_size_comp, ssc_inputs())
})

.summary_sample_size_comp <- reactive({
  if (is.null(input$ssc_type)) return(invisible())
  summary(.sample_size_comp())
})

observeEvent(input$sample_size_comp_report, {
  update_report(inp_main = clean_args(ssc_inputs(), ssc_args),
                fun_name = "sample_size_comp", outputs = "summary", figs = FALSE)
})
