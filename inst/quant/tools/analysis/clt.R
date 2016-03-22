###############################
# Central Limit Theorem
###############################
clt_dist <- c("Normal" = "rnorm", "Binomial" = "binom", "Uniform" = "runif", "Exponential" = "expo")
clt_stat <- c("Sum" = "Sum", "Mean" = "Mean")

output$ui_clt <- renderUI({
  tagList(
    wellPanel(
      actionButton("clt_resample", "Sample", width = "100%")
    ),
    wellPanel(
      selectInput(inputId = "clt_dist", label = "Distribution:", choices = clt_dist,
        selected = state_single("clt_dist", clt_dist), multiple = FALSE),
      conditionalPanel(condition = "input.clt_dist == 'runif'",
      #   with(tags, table(
      #     td(numericInput("clt_unif_min", "Min:", value = state_init("clt_unif_min", 0))),
      #     td(numericInput("clt_unif_max", "Max:", value = state_init("clt_unif_max", 1)))
      #   ))
      # ),
        div(class="row",
          div(class="col-xs-6",
            numericInput("clt_unif_min", "Min:", value = state_init("clt_unif_min", 0))
          ),
          div(class="col-xs-6",
            numericInput("clt_unif_max", "Max:", value = state_init("clt_unif_max", 1))
          )
        )
      ),
      conditionalPanel(condition = "input.clt_dist == 'rnorm'",
        div(class="row",
          div(class="col-xs-6",
            numericInput("clt_norm_mean", "Mean:", value = state_init("clt_norm_mean", 0))
          ),
          div(class="col-xs-6",
            numericInput("clt_norm_sd", "SD:", value = state_init("clt_norm_sd", 1), min = 0.001)
          )
        )
      ),
      conditionalPanel(condition = "input.clt_dist == 'expo'",
        numericInput("clt_expo_rate", "Rate:", value = state_init("clt_expo_rate", 1), min = 1, step = 1)
      ),
      conditionalPanel(condition = "input.clt_dist == 'binom'",
        div(class="row",
          div(class="col-xs-6",
            numericInput("clt_binom_size", "Size:", value = state_init("clt_binom_size", 5), min = 1, max = 100, step = 1)
          ),
          div(class="col-xs-6",
            numericInput("clt_binom_prob", "Prob:", value = state_init("clt_binom_prob", 0.9), min = 0.01, max = 1, step = .05)
          )
        )
      ),

      div(class="row",
          div(class="col-xs-6",
            numericInput("clt_n", "Sample size:",  value = state_init("clt_n", 100), min = 2, step = 1)
          ),
          div(class="col-xs-6",
            numericInput("clt_m", "# of samples:",  value = state_init("clt_m", 100), min = 2, step = 1)
          )
      ),
      radioButtons("clt_stat", label = NULL, choices = clt_stat, selected = state_init("clt_stat", "Sum"),
                   inline = TRUE)
    ),
    help_modal('Central Limit Theorem', 'clt_help', inclMD(file.path(r_path,"quant/tools/help/clt.md")))
 	)
})

clt_plot_width <- function() 700

clt_plot_height <- function() 700

## output is called from the main radiant ui.R
output$clt <- renderUI({
    # register_print_output("summary_clt", ".summary_clt")
    register_plot_output("plot_clt", ".plot_clt",
                          height_fun = "clt_plot_height",
                          width_fun = "clt_plot_width")

    ## two separate tabs
    clt_output_panels <- tagList(
      # tabPanel("Summary", verbatimTextOutput("summary_prob_calc")),
      tabPanel("Plot",
        plot_downloader("clt", width = clt_plot_width(), height = clt_plot_height()),
        plotOutput("plot_clt", width = "100%", height = "100%")
      )
    )

    stat_tab_panel(menu = "Base",
                   tool = "Central Limit Theorem",
                   data = NULL,
                   tool_ui = "ui_clt",
                   output_panels = clt_output_panels)

})

.clt<- eventReactive(input$clt_resample, {

  if(is.null(input$clt_dist)) return("Please choose a distribution")

  ## avoiding input errors
  if(is.na(input$clt_n) || input$clt_n < 2)
    return("Please choose a sample size larger than 2.")
  if(is.na(input$clt_m) || input$clt_m < 2)
    return("Please choose 2 or more samples.")
  if(is.na(input$clt_unif_min))
    return("Please choose a minimum value for the uniform distribution.")
  if(is.na(input$clt_unif_max))
    return("Please choose a maximum value for the uniform distribution.")
  if(is.na(input$clt_norm_mean))
    return("Please choose a mean value for the normal distribution.")
  if(is.na(input$clt_norm_sd) || input$clt_norm_sd < .001)
    return("Please choose a non-zero standard deviation for the normal distribution.")
  if(is.na(input$clt_expo_rate) || input$clt_expo_rate < 1)
    return("Please choose a rate larger than 1 for the exponential distribution.")
  if(is.na(input$clt_binom_size) || input$clt_binom_size < 1)
    return("Please choose a size parameter larger than 1 for the binomial distribution.")
  if(is.na(input$clt_binom_prob) || input$clt_binom_prob < 0.01)
    return("Please choose a probability between 0 and 1 for the binomial distribution.")

  ## creating a dependency so a new set of draw is generated every time the button is pressed
  # input$clt_resample

	clt(input$clt_dist, input$clt_n, input$clt_m, input$clt_stat)
})

clt <- function(clt_dist, clt_n, clt_m, clt_stat) {

  n <- clt_n; m <- clt_m; dist <- clt_dist
  if(clt_dist == "runif") {
    matrix(runif(n*m, min=input$clt_unif_min, max=input$clt_unif_max), n, m)
  } else if (clt_dist == "rnorm") {
    matrix(rnorm(n*m, mean = input$clt_norm_mean, sd = input$clt_norm_sd), n, m)
  } else if (clt_dist == "expo") {
    matrix(rexp(n*m, rate = input$clt_expo_rate), n, m)
  } else if (clt_dist == "binom") {
    matrix(rbinom(n*m, size = input$clt_binom_size, prob=input$clt_binom_prob), n, m)
  }
}

.plot_clt <- function(result = .clt()) {

  if (not_pressed(input$clt_resample)) return("** Press the Sample button to simulate data **")

  if(result %>% is.character) return(result)

  clt_stat <- input$clt_stat
  if (is.null(clt_stat)) return()
  if(clt_stat == "Sum") {
    sstat <- data.frame("Sum" = colSums(result))
  } else {
    sstat <- data.frame("Mean" = colMeans(result))
  }

  m <- dim(result)[2]
  data1 <- data.frame(sample_1 = result[,1])
  datam <- data.frame(sample_m = result[,m])

  plots <- list()

  plots[[1]] <- visualize(data1, xvar = "sample_1", bins = 10, custom = TRUE) +
                  xlab("Sample #1")

  plots[[2]] <- visualize(datam, xvar = "sample_m", bins = 10, custom = TRUE) +
                  xlab(paste0("Sample #", m))

  plots[[3]] <- visualize(sstat, xvar = clt_stat, bins = 10, custom = TRUE)

  plots[[4]] <- visualize(sstat, xvar = clt_stat, type = "density", custom = TRUE) +
                  stat_function(fun = dnorm, args = list(mean = mean(sstat[[1]]),
                                sd = sd(sstat[[1]])), color = "black", size = 1)

  withProgress(message = 'Making plots', value = 0, {
    do.call(gridExtra::arrangeGrob, c(plots, list(ncol = min(2,length(plots)))))
  })

}
