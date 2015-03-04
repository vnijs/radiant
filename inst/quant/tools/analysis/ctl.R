###############################
# Central Limit Theorem
###############################

# out <- render('report.Rmd', switch(
#   input$format,
#   PDF = pdf_document(), HTML = html_document(), Word = word_document()
# ))

ctl_dist <- c("Uniform" = "runif", "Normal" = "rnorm", "Exponential" = "expo","Binomial" = 'binom')
ctl_stat <- c("Sum" = "Sum", "Mean" = "Mean")

output$ui_ctl <- renderUI({
  list(
    wellPanel(
      selectInput(inputId = "ctl_dist", label = "Distribution (select one):", choices = ctl_dist,
        selected = state_single("ctl_dist", ctl_dist), multiple = FALSE),
      conditionalPanel(condition = "input.ctl_dist == 'runif'",
        div(class="row",
          div(class="col-xs-6",
            numericInput("ctl_unif_min", "Min:", value = 0, min = -10, max = 0, step = 1)
          ),
          div(class="col-xs-6",
            numericInput("ctl_unif_max", "Max:", value = 1, min = 1, max = 10, step = 1)
          )
        )
      ),
      conditionalPanel(condition = "input.ctl_dist == 'rnorm'",
        div(class="row",
          div(class="col-xs-6",
            numericInput("ctl_norm_mean", "Mean:", value = 0)
          ),
          div(class="col-xs-6",
            numericInput("ctl_norm_sd", "SD:", value = 1, min = 0.001)
          )
        )
      ),
      conditionalPanel(condition = "input.ctl_dist == 'expo'",
        numericInput("ctl_expo_rate", "Rate:", value = 1, min = 1, step = 1)
      ),
      conditionalPanel(condition = "input.ctl_dist == 'binom'",
        div(class="row",
          div(class="col-xs-6",
            numericInput("ctl_binom_size", "Size:", value = 1, min = 1, max = 100, step = 1)
          ),
          div(class="col-xs-6",
            numericInput("ctl_binom_prob", "Prob:", value = 0.15, min = 0.01, max = 1, step = .05)
          )
        )
      ),

      div(class="row",
          div(class="col-xs-6",
            numericInput("ctl_n", "Sample size:",  value = 50, min = 2, max = 500, step = 1)
          ),
          div(class="col-xs-6",
            numericInput("ctl_m", "# of samples:",  value = 100, min = 2, max = 500, step = 1)
          )
      ),
      radioButtons("ctl_stat", label = NULL, choices = ctl_stat, selected = "Mean",
                   inline = TRUE),
		  actionButton("ctl_resample", "Resample")
		),
# 	 	helpAndReport('Central Limit Theorem','ctl',inclRmd("../quant/tools/help/ctl.Rmd"))
		helpModal('Central Limit Theorem','ctlHelp',inclMD("../quant/tools/help/ctl.md"))
 	)
})

output$ctl <- renderUI({
	# for input-output
  statTabPanel("Random", "Central Limit Theorem", ".ctl", "ctl")
})

.ctl<- reactive({

  if(input$ctl_dist %>% is.null) return("Please choose a distribution")

  # avoiding input errors
  # if(input$ctl_n %>% { is.na(.) | . < 2 })
  if(is.na(input$ctl_n) | input$ctl_n < 2)
    return("Please choose a sample size larger than 2.")
  if(is.na(input$ctl_m) | input$ctl_m < 2)
    return("Please choose 2 or more samples.")
  if(is.na(input$ctl_unif_min))
    return("Please choose a minimum value for the uniform distribution.")
  if(is.na(input$ctl_unif_max))
    return("Please choose a maximum value for the uniform distribution.")
  if(is.na(input$ctl_norm_mean))
    return("Please choose a mean value for the normal distribution.")
  if(is.na(input$ctl_norm_sd) | input$ctl_norm_sd < .001)
    return("Please choose a non-zero standard deviation for the normal distribution.")
  if(is.na(input$ctl_expo_rate) | input$ctl_expo_rate < 1)
    return("Please choose a rate larger than 1 for the exponential distribution.")
  if(is.na(input$ctl_binom_size) | input$ctl_binom_size < 1)
    return("Please choose a size parameter larger than 1 for the binomial distribution.")
  if(is.na(input$ctl_binom_prob) | input$ctl_binom_prob < 0.01)
    return("Please choose a probability between 0 and 1 for the binomial distribution.")

  # creating a dependency so a new set of draw is generated every time the button is pressed
  input$ctl_resample

	ctl(input$ctl_dist, input$ctl_n, input$ctl_m, input$ctl_stat)
})

 observe({
  if(input$ctlReport %>% not_pressed) return()
  isolate({
		inp <- list(input$ctl_dist, input$ctl_n, input$ctl_m, input$ctl_stat)
		updateReport(inp,"ctl")
  })
})

ctl <- function(ctl_dist, ctl_n, ctl_m, ctl_stat) {

  n <- ctl_n; m <- ctl_m; dist <- ctl_dist
  if(ctl_dist == "runif") {
    data <- matrix(runif(n*m, min=input$ctl_unif_min, max=input$ctl_unif_max),
                         n, m)
  } else if (ctl_dist == "rnorm") {
    data <- matrix(rnorm(n*m, mean = input$ctl_norm_mean,
                         sd = input$ctl_norm_sd), n, m)
  } else if (ctl_dist == "expo") {
    data <- matrix(rexp(n*m, rate = input$ctl_expo_rate), n, m)
  } else if (ctl_dist == "binom") {
    data <- matrix(rbinom(n*m, size = input$ctl_binom_size,
                          prob=input$ctl_binom_prob), n, m)
  }

  data

}

summary_ctl <- function(result = .ctl())
  cat("See the Plots tab for output")

plots_ctl <- function(result = .ctl()) {

  if(result %>% is.character) return(result)

  ctl_stat <- input$ctl_stat
  if(ctl_stat == "Sum") {
    sstat <- data.frame("Sum" = colSums(result))
  } else {
    sstat <- data.frame("Mean" = colMeans(result))
  }

  m <- dim(result)[2]
  sample_m <-paste0("Sample_",m)
  data1 <- data.frame("Sample_1" = result[,1])
  datam <- data.frame(sample_m = result[,m])
  colnames(datam) <- sample_m

  bw <- diff(range(sstat, na.rm = TRUE)) / 10
  bwd1 <- diff(range(data1, na.rm = TRUE)) / 10
  bwdm <- diff(range(datam, na.rm = TRUE)) / 10

  plots <- list()
  plots[[1]] <- ggplot(data1, aes_string(x="Sample_1")) +
                  geom_histogram(binwidth = bwd1)
  plots[[2]] <- ggplot(datam, aes_string(x=sample_m)) +
                  geom_histogram(binwidth = bwdm)
  plots[[3]] <- ggplot(sstat, aes_string(x=ctl_stat)) +
                  geom_histogram(binwidth = bw)
  plots[[4]] <- ggplot(sstat, aes_string(x=ctl_stat)) +
                  geom_density(alpha=.3, fill = "green") +
                  stat_function(fun = dnorm, args = list(mean = mean(sstat[,1]),
                                sd = sd(sstat[,1])), color = "blue") +
                                labs(y = "") + theme(axis.text.y = element_blank())

  withProgress(message = 'Making plots', value = 0, {
    do.call(grid.arrange, c(plots, list(ncol = min(2,length(plots)))))
  })

}
