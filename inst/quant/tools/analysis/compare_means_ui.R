# choice lists for compare means
cm_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
cm_paired <- c("independent" = "independent", "paired" = "paired")
cm_adjust <- c("None" = "none", "Bonferroni" = "bonf")
cm_plots <- c("Bar" = "bar", "Box" = "box", "Density" = "density")

# list of function arguments
cm_args <- as.list(formals(compare_means))

# list of function inputs selected by user
cm_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(cm_args))
    cm_args[[i]] <- input[[i]]
  cm_args
})

###############################
# Compare means
###############################
output$ui_cm_var1 <- renderUI({
  isNumOrFct <- "numeric" == getdata_class() |
                "integer" == getdata_class() |
                "factor" == getdata_class()
  vars <- varnames()[isNumOrFct]
  selectInput(inputId = "cm_var1",
              label = "Select a factor or numeric variable:",
              choices = vars,
              selected = state_singlevar("cm_var1",vars),
              multiple = FALSE)
})

output$ui_cm_var2 <- renderUI({
  if(input$cm_var1 %>% not_available) return()
  isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  if(input$cm_var1 %in% vars) {
    # when cm_var1 is numeric comparisons for multiple variables are possible
    vars <- vars[-which(vars == input$cm_var1)]
    if(length(vars) == 0) return()
    selectizeInput(inputId = "cm_var2", label = "Variables (select one or more):",
                choices = vars,
                selected = state_multvar("cm_var2", vars),
                multiple = TRUE,
                options = list(placeholder = 'Select variables',
                               plugins = list('remove_button', 'drag_drop')))
  } else {
    # when cm_var1 is not numeric comparisons are across levels/groups
    selectInput(inputId = "cm_var2", label = "Variables (select one):",
                choices = vars, selected = state_singlevar("cm_var2",vars),
                multiple = FALSE)
  }
})

output$ui_compare_means <- renderUI({
  tagList(
    wellPanel(
      conditionalPanel(condition = "input.tabs_compare_means == 'Plot'",
        selectizeInput(inputId = "cm_plots", label = "Select plots:",
                choices = cm_plots,
                selected = state_init_list("cm_plots", cm_args$cm_plots, cm_plots),
                multiple = TRUE,
                options = list(plugins = list('remove_button', 'drag_drop')))
      ),
      uiOutput("ui_cm_var1"),
      uiOutput("ui_cm_var2"),
      conditionalPanel(condition = "input.tabs_compare_means == 'Summary'",
        selectInput(inputId = "cm_alternative", label = "Alternative hypothesis:",
                    choices = cm_alt,
                    selected = state_init_list("cm_alternative",
                                               cm_args$cm_alternative,
                                               cm_alt)),
        sliderInput('cm_sig_level',"Significance level:", min = 0.85, max = 0.99,
          value = state_init('cm_sig_level',cm_args$cm_sig_level), step = 0.01),
        radioButtons(inputId = "cm_paired", label = "Sample type:", cm_paired,
          selected = state_init_list("cm_paired", cm_args$cm_paired, cm_paired),
          inline = TRUE),
        radioButtons(inputId = "cm_adjust", label = "Multiple comp. adjustment:", cm_adjust,
          selected = state_init_list("cm_adjust", cm_args$cm_adjust, cm_adjust),
          inline = TRUE)
      )
    ),
    help_and_report(modal_title = 'Compare means',
                    fun_name = 'compare_means',
                    help_file = inclMD("../quant/tools/help/compare_means.md"))
  )
})

cm_plot_height <- function() {
  result <- .compare_means()
  ifelse(!"character" %in% class(result), result$plot_height, 650)
}

# output is called from the main radiant ui.R
output$compare_means <- renderUI({

    register_print_output("summary_compare_means", ".compare_means", )
    register_plot_output("plot_compare_means", ".compare_means",
                         height_fun = "cm_plot_height")

    # two separate tabs
    cm_output_panels <- tabsetPanel(
      id = "tabs_compare_means",
      tabPanel("Summary", verbatimTextOutput("summary_compare_means")),
      tabPanel("Plot", plotOutput("plot_compare_means", height = "100%"))
    )

    statTabPanel2(menu = "Base",
                  tool = "Compare means",
                  tool_ui = "ui_compare_means",
                  output_panels = cm_output_panels)
})

.compare_means <- reactive({

  if(input$cm_var2 %>% not_available)
    return("This analysis requires at least two variables of type factor, numeric, or interval.\nIf less than two such variables are available please select another dataset")
  # cm_var2 may still have > elements selected when cm_var1 is changed to a factor
  if(length(input$cm_var2) > 1 && getdata_class()[input$cm_var1] == 'factor')
    return(" ")
  # cm_var2 may be equal to cm_var1 when changing cm_var1 from factor to numeric
  if(input$cm_var1 %in% input$cm_var2) return(" ")

  do.call(compare_means, cm_inputs())
})

observe({
  if(input$compare_means_report %>% not_pressed) return()
  isolate({
    update_report(inp = cm_inputs() %>% clean_args, fun_name = "compare_means",
                  outputs = c("summary", "plot"),
                  fig.height = round(7 * cm_plot_height()/650))
  })
})
