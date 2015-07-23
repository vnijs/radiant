## choice lists for compare means
cp_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
cp_adjust <- c("None" = "none", "Bonferroni" = "bonf")
cp_plots <- c("Proportions" = "props", "Counts" = "counts")

## list of function arguments
cp_args <- as.list(formals(compare_props))

## list of function inputs selected by user
cp_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  cp_args$data_filter <- if (input$show_filter) input$data_filter else ""
  cp_args$dataset <- input$dataset
  for (i in r_drop(names(cp_args)))
    cp_args[[i]] <- input[[paste0("cp_",i)]]
  cp_args
})

###############################
# Compare proportions
###############################
output$ui_cp_var1 <- renderUI({
  vars <- groupable_vars()
  selectInput(inputId = "cp_var1",
              label = "Select a grouping variable:",
              choices = vars,
              selected = state_single("cp_var1",vars),
              multiple = FALSE)
})

output$ui_cp_var2 <- renderUI({
  if (not_available(input$cp_var1)) return()
  vars <- two_level_vars()
  if (input$cp_var1 %in% vars) vars <- vars[-which(vars == input$cp_var1)]
  selectInput(inputId = "cp_var2", label = "Variable (select one):",
              choices = vars, selected = state_single("cp_var2",vars),
              multiple = FALSE)
})

output$ui_cp_levs <- renderUI({
  if (not_available(input$cp_var2))
    levs <- c()
  else
    levs <- .getdata()[1,input$cp_var2] %>% as.factor %>% levels

  selectInput(inputId = "cp_levs", label = "Choose level:",
              choices = levs,
              selected = state_single("cp_levs",levs), multiple = FALSE)
})

output$ui_compare_props <- renderUI({
  tagList(
    conditionalPanel(condition = "input.tabs_compare_props == 'Plot'",
      wellPanel(
        selectizeInput(inputId = "cp_plots", label = "Select plots:",
                choices = cp_plots,
                selected = state_multiple("cp_plots", cp_plots, "props"),
                multiple = TRUE,
                options = list(plugins = list('remove_button', 'drag_drop')))
      )
    ),
    wellPanel(
      uiOutput("ui_cp_var1"),
      uiOutput("ui_cp_var2"),
      uiOutput("ui_cp_levs"),
      conditionalPanel(condition = "input.tabs_compare_props == 'Summary'",
        selectInput(inputId = "cp_alternative", label = "Alternative hypothesis:",
                    choices = cp_alt,
                    selected = state_single("cp_alternative", cp_alt,
                                               cp_args$alternative)),
        sliderInput("cp_conf_lev","Significance level:", min = 0.85, max = 0.99,
          value = state_init("cp_conf_lev",cp_args$conf_lev), step = 0.01),
        radioButtons(inputId = "cp_adjust", label = "Multiple comp. adjustment:", cp_adjust,
          selected = state_init("cp_adjust", cp_args$adjust),
          inline = TRUE)
      )
    ),
    help_and_report(modal_title = "Compare proportions",
                    fun_name = "compare_props",
                    help_file = inclMD(file.path(r_path,"quant/tools/help/compare_props.md")))
  )
})

cp_plot <- reactive({
  list(plot_width = 650, plot_height = 400 * length(input$cp_plots))
})

cp_plot_width <- function()
  cp_plot() %>% { if (is.list(.)) .$plot_width else 650 }

cp_plot_height <- function()
  cp_plot() %>% { if (is.list(.)) .$plot_height else 400 }

# output is called from the main radiant ui.R
output$compare_props <- renderUI({

    register_print_output("summary_compare_props", ".summary_compare_props", )
    register_plot_output("plot_compare_props", ".plot_compare_props",
                         height_fun = "cp_plot_height")

    # two separate tabs
    cp_output_panels <- tabsetPanel(
      id = "tabs_compare_props",
      tabPanel("Summary", verbatimTextOutput("summary_compare_props")),
      tabPanel("Plot", plot_downloader("compare_props", height = cp_plot_height()),
               plotOutput("plot_compare_props", height = "100%"))
    )

    stat_tab_panel(menu = "Base",
                  tool = "Compare proportions",
                  tool_ui = "ui_compare_props",
                  output_panels = cp_output_panels)
})

.compare_props <- reactive({
  do.call(compare_props, cp_inputs())
})

.summary_compare_props <- reactive({

  if (not_available(input$cp_var1) || not_available(input$cp_var2))
    return("This analysis requires two categorical variables. The first can have multiple\nlevels. The second can have only two levels. If these\nvariables are not available please select another dataset")

  # cp_var2 may be equal to cp_var1 when changing cp_var1 to cp_var2
  if (input$cp_var1 %in% input$cp_var2) return(" ")

  summary(.compare_props())
})

.plot_compare_props <- reactive({

  if (not_available(input$cp_var1) || not_available(input$cp_var2))
    return("This analysis requires two categorical variables. The first can have multiple\nlevels. The second can have only two levels. If these\nvariables are not available please select another dataset")

  # cp_var2 may be equal to cp_var1 when changing cp_var1 to cp_var2
  if (input$cp_var1 %in% input$cp_var2) return(" ")

  plot(.compare_props(), plots = input$cp_plots, shiny = TRUE)
})

observe({
  if (not_pressed(input$compare_props_report)) return()
  isolate({
    if (length(input$cp_plots) == 0) {
      figs <- FALSE
      outputs <- c("summary")
      inp_out <- list("","")
    } else {
      outputs <- c("summary","plot")
      inp_out <- list(plots = input$cp_plots) %>% list("",.)
      figs <- TRUE
    }
    update_report(inp_main = clean_args(cp_inputs(), cp_args),
                  fun_name = "compare_props",
                  inp_out = inp_out, outputs = outputs, figs = figs,
                  fig.width = round(7 * cp_plot_width()/650,2),
                  fig.height = round(7 * cp_plot_height()/650,2))
  })
})
