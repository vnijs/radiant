pc_dist <- c("Binomial" = "binom", "Chi-squared" = "chisq", "Discrete" = "disc",
             "Exponential" = "expo", "F" = "fdist", "Normal" = "norm",
             "Poisson" = "pois", "t" = "tdist", "Uniform" = "unif")

pc_type <- c("Values" = "values", "Probabilities" = "probs")

output$ui_pc_pois <- renderUI({
  numericInput("pcp_lambda", label = "Lambda:", value = state_init("pcp_lambda", 1), min = 1)
})

output$ui_pc_input_pois   <- renderUI({

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pcp_lb", label = "Lower bound:",
                              value = state_init("pcp_lb", NA))),
        div(class="col-xs-6",numericInput("pcp_ub", label = "Upper bound:",
                             value = state_init("pcp_ub", NA)))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pcp_plb", label = "Lower bound:",
                              value = state_init("pcp_plb", NA), step = .005)),
        div(class="col-xs-6",numericInput("pcp_pub", label = "Upper bound:",
                             value = state_init("pcp_pub", 0.95), step = .005))
    )
  }
})

output$ui_pc_expo <- renderUI({
  numericInput("pce_rate", label = "Rate:", value = state_init("pce_rate", 1), min = 0)
})

output$ui_pc_input_expo   <- renderUI({

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pce_lb", label = "Lower bound:",
                              value = state_init("pce_lb", NA))),
        div(class="col-xs-6",numericInput("pce_ub", label = "Upper bound:",
                             value = state_init("pce_ub", NA)))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pce_plb", label = "Lower bound:",
                              value = state_init("pce_plb", NA), step = .005)),
        div(class="col-xs-6",numericInput("pce_pub", label = "Upper bound:",
                             value = state_init("pce_pub", 0.95), step = .005))
    )
  }
})

output$ui_pc_disc <- renderUI({
  tagList(
    returnTextInput("pcd_v", label = "Values:", value = state_init("pcd_v", "1 2 3 4 5 6")),
    returnTextInput("pcd_p", label = "Probabilities:", value = state_init("pcd_p", "1/6"))
  )
})

output$ui_pc_input_disc   <- renderUI({

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pcd_lb", label = "Lower bound:",
                              value = state_init("pcd_lb", NA))),
        div(class="col-xs-6",numericInput("pcd_ub", label = "Upper bound:",
                             value = state_init("pcd_ub", NA)))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pcd_plb", label = "Lower bound:",
                              value = state_init("pcd_plb", NA), step = .005)),
        div(class="col-xs-6",numericInput("pcd_pub", label = "Upper bound:",
                             value = state_init("pcd_pub", 0.95), step = .005))
    )
  }
})

output$ui_pc_fdist <- renderUI({
  tagList(
    numericInput("pcf_df1", label = "Degrees of freedom 1:", value = state_init("pcf_df1", 10), min = 1),
    numericInput("pcf_df2", label = "Degrees of freedom 2:", value = state_init("pcf_df2", 10), min = 1)
  )
})

output$ui_pc_input_fdist   <- renderUI({

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pcf_lb", label = "Lower bound:",
                              value = state_init("pcf_lb", NA))),
        div(class="col-xs-6",numericInput("pcf_ub", label = "Upper bound:",
                             value = state_init("pcf_ub", Inf)))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pcf_plb", label = "Lower bound:",
                              value = state_init("pcf_plb", NA), step = .005)),
        div(class="col-xs-6",numericInput("pcf_pub", label = "Upper bound:",
                             value = state_init("pcf_pub", 0.95), step = .005))
    )
  }
})

output$ui_pc_chisq <- renderUI({
  numericInput("pcc_df", label = "Degrees of freedom:", value = state_init("pcc_df", 1), min = 1)
})

output$ui_pc_input_chisq   <- renderUI({

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pcc_lb", label = "Lower bound:",
                              value = state_init("pcc_lb", -Inf))),
        div(class="col-xs-6",numericInput("pcc_ub", label = "Upper bound:",
                             value = state_init("pcc_ub", Inf)))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pcc_plb", label = "Lower bound:",
                              value = state_init("pcc_plb", NA), step = .005)),
        div(class="col-xs-6",numericInput("pcc_pub", label = "Upper bound:",
                             value = state_init("pcc_pub", 0.95), step = .005))
    )
  }
})

output$ui_pc_tdist <- renderUI({
  tagList(
    numericInput("pct_df", label = "Degrees of freedom:", value = state_init("pct_df", 10), min = 3)
    # , div(class="row",
    #     div(class="col-xs-6", numericInput("pct_mean", label = "Mean:",
    #                           value = state_init("pct_mean", 0))),
    #     div(class="col-xs-6",numericInput("pct_stdev", label = "St. dev:", min = 0,
    #                          value = state_init("pct_stdev", 1)))
    # )
  )
})

output$ui_pc_input_tdist <- renderUI({

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pct_lb", label = "Lower bound:",
                              value = state_init("pct_lb", -Inf))),
        div(class="col-xs-6",numericInput("pct_ub", label = "Upper bound:",
                             value = state_init("pct_ub", Inf)))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pct_plb", label = "Lower bound:",
                              value = state_init("pct_plb", .025), step = .005)),
        div(class="col-xs-6",numericInput("pct_pub", label = "Upper bound:",
                             value = state_init("pct_pub", 0.975), step = .005))
    )
  }
})

output$ui_pc_norm <- renderUI({
  tagList(
    div(class="row",
        div(class="col-xs-6", numericInput("pc_mean", label = "Mean:",
                              value = state_init("pc_mean", 0))),
        div(class="col-xs-6",numericInput("pc_stdev", label = "St. dev:", min = 0,
                             value = state_init("pc_stdev", 1)))
    )
  )
})

output$ui_pc_input_norm <- renderUI({

  # mean <- if (is_empty(input$mean)) NA else input$mean

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pc_lb", label = "Lower bound:",
                              value = state_init("pc_lb", -Inf))),
        div(class="col-xs-6",numericInput("pc_ub", label = "Upper bound:",
                             value = state_init("pc_ub", 0)))
                             # value = state_init("pc_ub", mean)))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pc_plb", label = "Lower bound:",
                              value = state_init("pc_plb", .025), step = .005)),
        div(class="col-xs-6",numericInput("pc_pub", label = "Upper bound:",
                             value = state_init("pc_pub", 0.975), step = .005))
    )
  }
})


output$ui_pc_binom <- renderUI({
  tagList(
    div(class="row",
        div(class="col-xs-6", numericInput("pcb_n", label = "n:",
                              value = state_init("pcb_n", 10)), min = 0),
        div(class="col-xs-6",numericInput("pcb_p", label = "p:", min = 0, max = 1,
                             value = state_init("pcb_p", .2), step = .005))
    )
  )
})

output$ui_pc_input_binom <- renderUI({

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pcb_lb", label = "Lower bound:",
                              value = state_init("pcb_lb", NA), min = 0, step = 1)),
        div(class="col-xs-6",numericInput("pcb_ub", label = "Upper bound:",
                             value = state_init("pcb_ub", 3), min = 0, step = 1))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pcb_plb", label = "Lower bound:",
                              value = state_init("pcb_plb", NA), min = 0, max = 1, step = .005)),
        div(class="col-xs-6",numericInput("pcb_pub", label = "Upper bound:",
                             value = state_init("pcb_pub", 0.3), min = 0, max = 1, step = .005))
    )
  }
})

output$ui_pc_unif <- renderUI({
  tagList(
    div(class="row",
        div(class="col-xs-6", numericInput("pcu_min", label = "Min:",
                              value = state_init("pcu_min", 0))),
        div(class="col-xs-6",numericInput("pcu_max", label = "Max:",
                             value = state_init("pcu_max", 1)))
    )
  )
})

output$ui_pc_input_unif <- renderUI({

  # mean <- if (is_empty(input$mean)) NA else input$mean

  if (input$pc_type == "values") {
    div(class="row",
        div(class="col-xs-6", numericInput("pcu_lb", label = "Lower bound:",
                              value = state_init("pcu_lb", NA))),
        div(class="col-xs-6",numericInput("pcu_ub", label = "Upper bound:",
                             value = state_init("pcu_ub", 0.3)))
                             # value = state_init("pc_ub", input$pc_mean)))
    )
  } else {
    div(class="row",
        div(class="col-xs-6", numericInput("pcu_plb", label = "Lower bound:",
                              value = state_init("pcu_plb", NA), step = .005)),
        div(class="col-xs-6",numericInput("pcu_pub", label = "Upper bound:",
                             value = state_init("pcu_pub", 0.3), step = .005))
    )
  }
})

output$ui_prob_calc <- renderUI({
  tagList(
    wellPanel(
      selectizeInput("pc_dist", label = "Distribution:",
        choices = pc_dist,
        selected = state_single("pc_dist", pc_dist, "norm"),
        multiple = FALSE),
      conditionalPanel("input.pc_dist == 'norm'",
        uiOutput("ui_pc_norm")
      ),
      conditionalPanel("input.pc_dist == 'binom'",
        uiOutput("ui_pc_binom")
      ),
      conditionalPanel("input.pc_dist == 'unif'",
        uiOutput("ui_pc_unif")
      ),
      conditionalPanel("input.pc_dist == 'tdist'",
        uiOutput("ui_pc_tdist")
      ),
      conditionalPanel("input.pc_dist == 'fdist'",
        uiOutput("ui_pc_fdist")
      ),
      conditionalPanel("input.pc_dist == 'chisq'",
        uiOutput("ui_pc_chisq")
      ),
      conditionalPanel("input.pc_dist == 'disc'",
        uiOutput("ui_pc_disc")
      ),
      conditionalPanel("input.pc_dist == 'expo'",
        uiOutput("ui_pc_expo")
      ),
      conditionalPanel("input.pc_dist == 'pois'",
        uiOutput("ui_pc_pois")
      )
    ),
    wellPanel(
      radioButtons("pc_type", label = "Input type:", choices = pc_type,
                   selected = state_init("pc_type", "values"),
                   inline = TRUE),
      conditionalPanel("input.pc_dist == 'norm'",
        uiOutput("ui_pc_input_norm")
      ),
      conditionalPanel("input.pc_dist == 'binom'",
        uiOutput("ui_pc_input_binom")
      ),
      conditionalPanel("input.pc_dist == 'unif'",
        uiOutput("ui_pc_input_unif")
      ),
      conditionalPanel("input.pc_dist == 'tdist'",
        uiOutput("ui_pc_input_tdist")
      ),
      conditionalPanel("input.pc_dist == 'fdist'",
        uiOutput("ui_pc_input_fdist")
      ),
      conditionalPanel("input.pc_dist == 'chisq'",
        uiOutput("ui_pc_input_chisq")
      ),
      conditionalPanel("input.pc_dist == 'disc'",
        uiOutput("ui_pc_input_disc")
      ),
      conditionalPanel("input.pc_dist == 'expo'",
        uiOutput("ui_pc_input_expo")
      ),
      conditionalPanel("input.pc_dist == 'pois'",
        uiOutput("ui_pc_input_pois")
      ),
      numericInput("pc_dec", label = "Decimals:",
                   value = state_init("pc_dec", 3), min = 0)
    ),
    help_and_report(modal_title = 'Probability calculator', fun_name = 'prob_calc',
                    help_file = inclMD(file.path(r_path,"quant/tools/help/prob_calc.md")))
  )
})

pc_plot_width <- function() 700

pc_plot_height <- function() 400

pc_args <- reactive({
  pc_dist <- input$pc_dist
  if (is_empty(pc_dist) || pc_dist == "norm") {
    as.list(formals(prob_norm))
  } else if (pc_dist == "binom") {
    as.list(formals(prob_binom))
  } else if (pc_dist == "unif") {
    as.list(formals(prob_unif))
  } else if (pc_dist == "tdist") {
    as.list(formals(prob_tdist))
  } else if (pc_dist == "fdist") {
    as.list(formals(prob_fdist))
  } else if (pc_dist == "chisq") {
    as.list(formals(prob_chisq))
  } else if (pc_dist == "disc") {
    as.list(formals(prob_disc))
  } else if (pc_dist == "expo") {
    as.list(formals(prob_expo))
  } else if (pc_dist == "pois") {
    as.list(formals(prob_pois))
  }
})

## list of function inputs selected by user
pc_inputs <- reactive({

  pc_dist <- input$pc_dist
  if (is_empty(pc_dist) || pc_dist == "norm") {
    pre <- "pc_"
  } else if (pc_dist == "binom") {
    pre <- "pcb_"
  } else if (pc_dist == "unif") {
    pre <- "pcu_"
  } else if (pc_dist == "tdist") {
    pre <- "pct_"
  } else if (pc_dist == "fdist") {
    pre <- "pcf_"
  } else if (pc_dist == "chisq") {
    pre <- "pcc_"
  } else if (pc_dist == "disc") {
    pre <- "pcd_"
  } else if (pc_dist == "expo") {
    pre <- "pce_"
  } else if (pc_dist == "pois") {
    pre <- "pcp_"
  }

  # loop needed because reactive values don't allow single bracket indexing
  args <- pc_args()
  for (i in names(args)) {
    args[[i]] <- input[[paste0(pre,i)]]
  }

  args[["dec"]] <- input$pc_dec

  args
})

## output is called from the main radiant ui.R
output$prob_calc <- renderUI({

    register_print_output("summary_prob_calc", ".summary_prob_calc")
    register_plot_output("plot_prob_calc", ".plot_prob_calc",
                          height_fun = "pc_plot_height",
                          width_fun = "pc_plot_width")

    ## two separate tabs
    pc_output_panels <- tagList(
      tabPanel("Summary", verbatimTextOutput("summary_prob_calc")),
      tabPanel("Plot",
               plot_downloader("prob_calc", height = pc_plot_height()),
               plotOutput("plot_prob_calc", width = "100%", height = "100%"))
    )

    stat_tab_panel(menu = "Base",
                   tool = "Probability calculator",
                   data = NULL,
                   tool_ui = "ui_prob_calc",
                   output_panels = pc_output_panels)

})

pc_available <- reactive({

  if (is_empty(input$pc_dist) || is_empty(input$pc_type)) {
    ""
  } else {
    a <- "available"
    if (input$pc_dist == "norm") {
      if (is_not(input$pc_mean) || is_not(input$pc_stdev))
        a <- "Please provide a mean and standard deviation"
    } else if (input$pc_dist == "binom") {
      if (is_not(input$pcb_n) || is_not(input$pcb_p))
        a <- "Please provide a value for n (number of trials) and p (probability of success)"
    } else if (input$pc_dist == "unif") {
      if (is_not(input$pcu_min) || is_not(input$pcu_max))
        a <- "Please provide a minimum and a maxium value"
    } else if (input$pc_dist == "tdist") {
      if (is_not(input$pct_df))
        a <- "Please provide a value for the degrees of freedom"
    } else if (input$pc_dist == "fdist") {
      if (is_not(input$pcf_df1) || is_not(input$pcf_df2))
        a <- "Please provide a value for the degrees of freedom"
    } else if (input$pc_dist == "chisq") {
      if (is_not(input$pcc_df))
        a <- "Please provide a value for the degrees of freedom"
    } else if (input$pc_dist == "disc") {
      if (is_empty(input$pcd_v) || is_empty(input$pcd_p))
        a <- "Please provide a set of values and probabilities. Separate numbers using spaces"
    } else if (input$pc_dist == "expo") {
      if (is_not(input$pce_rate) || input$pce_rate <= 0)
        a <- "Please provide a value for the rate (> 0)"
    } else if (input$pc_dist == "pois") {
      if (is_not(input$pcp_lambda) || input$pcp_lambda <= 0)
        a <- "Please provide a value for lambda (> 0)"
    } else {
      a <- ""
    }
    a
  }
})

.prob_calc <- reactive({
  do.call(get(paste0("prob_",input$pc_dist)), pc_inputs())
})

.summary_prob_calc <- reactive({
  if (pc_available() != "available") return(pc_available())
  type <- if (is.null(input$pc_type)) "values" else input$pc_type
  summary(.prob_calc(), type = type)
})

.plot_prob_calc <- reactive({
  if (pc_available() != "available") return(pc_available())
  type <- if (is.null(input$pc_type)) "values" else input$pc_type
  plot(.prob_calc(), type = type, shiny = TRUE)
})

observeEvent(input$prob_calc_report, {
  type <- input$pc_type
  inp <- pc_inputs()
  if (!is.null(type) && type == "probs") {
    inp_out <- list(type = type) %>% list(.,.)
    inp[["ub"]] <- inp[["lb"]] <- NA
  } else {
    inp_out <- list("","")
    inp[["pub"]] <- inp[["plb"]] <- NA
  }
  outputs <- c("summary","plot")
  update_report(inp_main = clean_args(inp, pc_args()),
                fun_name = paste0("prob_",input$pc_dist),
                inp_out = inp_out,
                outputs = outputs,
                figs = TRUE,
                fig.width = round(7 * pc_plot_width()/650,2),
                fig.height = round(7 * pc_plot_height()/650,2))
})
