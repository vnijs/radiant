###############################
# Sampling and assignment
###############################
output$uiRnd_var <- renderUI({
  vars <- varnames()
	isChar <- "character" == getdata_class()
 	vars <- vars[isChar]
  if(length(vars) == 0) return()
  selectInput(inputId = "rnd_var", label = "Variable (select one):", choices = vars,
  	selected = state_singlevar("rnd_var",vars), multiple = FALSE)
})

output$uiRnd_block <- renderUI({
  vars <- varnames()
	isFct <- "factor" == getdata_class()
 	vars <- vars[isFct]
  if(length(vars) == 0) return()
  vars <- c("None",vars)
  selectInput(inputId = "rnd_block", label = "Block variable (select one):", choices = vars,
  	selected = state_singlevar("rnd_block",vars), multiple = FALSE)
})

rnd_sample <- c("Sample" = "sample", "Assign" = "assign")

output$ui_random <- renderUI({
  list(
  	wellPanel(
	 	 	uiOutput("uiRnd_var"),
		  radioButtons(inputId = "rnd_sample", label = "", rnd_sample,
	  	  selected = state_init_list("rnd_sample","sample", rnd_sample)),
		  conditionalPanel(condition = "input.rnd_sample == 'sample'",
		  	numericInput("rnd_sample_size", "Sample size:", min = 1,
		  		value = state_init("rnd_sample_size",1))
	  	),
		  conditionalPanel(condition = "input.rnd_sample != 'sample'",
		  	numericInput("rnd_nrCond", "Number of conditions:", min = 2,
		  		value = state_init("rnd_nrCond",2)),
		 	 	uiOutput("uiRnd_block"),
		    actionButton("rnd_save_treatment", "Save treatment")
	  	)
  	),
	 	helpAndReport('Random','random',inclMD("../quant/tools/help/random.md"))
 	)
})

output$random <- renderUI({
	# for input-output
  statTabPanel("Random", "Random", ".random", "random")
})

.random <- reactive({
	# reactive that calls the function for main analysis
	# . used to indicate this is an 'internal' function

	ret_text <- "This analysis requires a variable of type character.\nEntries should be unique (i.e., no duplicates).\nPlease select another dataset."
	if(is.null(input$rnd_var)) return(ret_text)
	# if(is.null(inChecker(c(input$rnd_var)))) return(ret_text)

	rvar <- getdata()$rnd_var
	if(length(unique(rvar)) != length(rvar)) return(ret_text)

	if(is.na(input$rnd_sample_size)) return("Please select a sample size of 1 or greater.")

	random(input$datasets, input$rnd_var, input$rnd_sample, input$rnd_sample_size,
			input$rnd_nrCond, input$rnd_block)
})

observe({
  if(is.null(input$randomReport) || input$randomReport == 0) return()
  isolate({
		inp <- list(input$datasets, input$rnd_var, input$rnd_sample, input$rnd_sample_size,
			input$rnd_nrCond, input$rnd_block)

		xcmd <- paste0("saveTreatmentAssign(result)")
		updateReport(inp,"random", xcmd = xcmd)
  })
})

random <- function(datasets, rnd_var, rnd_sample, rnd_sample_size, rnd_nrCond, rnd_block) {

	# example list of names obtained from http://listofrandomnames.com
	dat <- values[[datasets]]

	if(rnd_sample == 'sample') {

		selDat <- dat[sample(1:nrow(dat), rnd_sample_size),, drop = FALSE]
		return(list(sample = selDat, dat = dat))
	} else {

		dat$treatment <- 0
		nrCond <<- rnd_nrCond 	# <<- needed else ddply doesn't know where to 'look'
		if(!is.null(rnd_block) && rnd_block != "None") {
			# adapted from http://stackoverflow.com/questions/5399773/how-to-generate-a-random-treatment-variable-by-factor
		 	dat <- ddply(dat, c(rnd_block), transform,
				treatment = replace(treatment, sample(seq_along(treatment)),
					rep(sample(1:nrCond), ceiling(length(treatment)/nrCond))[1:length(treatment)]))
		} else {
			dat$treatment <- replace(dat$treatment, sample(seq_along(dat$treatment)), 1:nrCond)
		}
		nrCond <<- NULL
		return(list(dat = dat))
	}
}

summary_random <- function(result = .random()) {
	if(!is.null(result$sample)) {
		cat("Selected units:\n")
		print(result$sample)
		cat("\nAll units:\n")
		print(result$dat)
	} else {
		cat("Assigned:\n")
		print(result$dat)
	}
}

plots_random <- function(result = .random()) {

	if(!is.null(result$sample)) {
		result <- "Relevant output is in the Summary tab."
		return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
	} else {
		result$dat$treatment <- as.factor(result$dat$treatment)
		p <- ggplot(result$dat, aes_string(x='treatment', fill='treatment')) + geom_bar(alpha=.3) +
			theme(legend.position = "none")
		print(p)
	}
}

saveTreatmentAssign <- function(result = .random()) {
	changedata(data.frame(as.factor(result$dat$treatment)), "treatment")
}

observe({
	if(is.null(input$rnd_save_treatment) || input$rnd_save_treatment == 0) return()
	isolate({
		result <- .random()
		if(is.character(result)) return()
		saveTreatmentAssign(result)
	})
})

###############################
# Sample size
###############################
rnd_mean <- c("Mean" = "mean", "Proportion" = "proportion")
rnd_pop_correction <- c("Yes" = "yes", "No" = "no")

output$ui_sampleSize <- renderUI({
  list(
  	wellPanel(
		  radioButtons(inputId = "rnd_mean", label = "", rnd_mean,
	  	  selected = state_init_list("rnd_mean","mean", rnd_mean)),
		  conditionalPanel(condition = "input.rnd_mean == 'mean'",
		    numericInput("rnd_mean_err", "Acceptable Error (units, e.g., $10):", min = 0,
		  		value = state_init("rnd_mean_err",.2), step = .1),
		    numericInput("rnd_mean_s", "Sample std. deviation:", min = 0,
		  		value = state_init("rnd_mean_s",3), step = .1)
	  	),
		  conditionalPanel(condition = "input.rnd_mean != 'mean'",
		  	numericInput("rnd_prop_err", "Acceptable Error (e.g., .03):", min = 0, max = 1,
		  		value = state_init("rnd_prop_err",.1), step = .01),
		    numericInput("rnd_prop_p", "Sample proportion:", min = 0, max = 1,
		  		value = state_init("rnd_prop_p",.5), step = .05)
	  	),
	    numericInput("rnd_z", "Confidence level (z-value):", min = 0,
	  		value = state_init("rnd_z",1.96), step = .1),
	    numericInput("rnd_incidence", "Incidence rate:", min = 0, max = 1,
	  		value = state_init("rnd_incidence",1), step = .05),
	    numericInput("rnd_response", "Response rate:", min = 0, max = 1,
	  		value = state_init("rnd_response",1), step = .05),
		  radioButtons(inputId = "rnd_pop_correction", label = "Correct for population size:",
		  	rnd_pop_correction, selected = state_init_list("rnd_pop_correction","no", rnd_pop_correction)),
		  conditionalPanel(condition = "input.rnd_pop_correction == 'yes'",
		    numericInput("rnd_pop_size", "Population size:", min = 1,
		  		value = state_init("rnd_pop_size",10^6), step = 1000))
		),
	 	helpAndReport('Sample size','sampleSize',inclMD("../quant/tools/help/sampleSize.md"))
 	)
})

output$sampleSize <- renderUI({
	# for input-output
  statTabPanel("Random", "Sample size", ".sampleSize", "sampleSize")
})

.sampleSize <- reactive({
	if(is.null(input$rnd_mean)) return("")
	sampleSize(input$rnd_mean, input$rnd_mean_err, input$rnd_mean_s, input$rnd_prop_err,
			input$rnd_prop_p, input$rnd_z, input$rnd_incidence, input$rnd_response, input$rnd_pop_correction,
			input$rnd_pop_size)
})

observe({
  if(is.null(input$sampleSizeReport) || input$sampleSizeReport == 0) return()
  isolate({
		inp <- list(input$rnd_mean, input$rnd_mean_err, input$rnd_mean_s, input$rnd_prop_err,
			input$rnd_prop_p, input$rnd_z, input$rnd_incidence, input$rnd_response, input$rnd_pop_correction,
			input$rnd_pop_size)
		updateReport(inp,"sampleSize")
  })
})

sampleSize <- function(rnd_mean, rnd_mean_err, rnd_mean_s, rnd_prop_err, rnd_prop_p, rnd_z, rnd_incidence,
		rnd_response, rnd_pop_correction, rnd_pop_size) {

	if(rnd_mean == 'mean') {

		if(is.na(rnd_mean_err)) return("Please select an error value greater 0.")
		n <- (rnd_z^2 * rnd_mean_s^2) / rnd_mean_err^2

		if(rnd_pop_correction == 'yes') n <- n * rnd_pop_size / ((n - 1) + rnd_pop_size)
		result <- list(n = ceiling(n))

	} else {

		if(is.na(rnd_prop_err)) return("Please select an error value greater 0.")
		n <- (rnd_z^2 * rnd_prop_p * (1 - rnd_prop_p)) / rnd_prop_err^2

		if(rnd_pop_correction == 'yes') n <- n * rnd_pop_size / ((n - 1) + rnd_pop_size)
		result <- list(n = ceiling(n))
	}

	result$rnd_incidence <- rnd_incidence
	result$rnd_response <- rnd_response
	result
}

summary_sampleSize <- function(result = .sampleSize()) {
	cat("Required sample size:", result$n)
	cat("\nRequired contact attempts:", result$n / result$rnd_incidence / result$rnd_response)
	cat("\n\nChoose a Z-value as follows:\n")
	cat("90%\t1.64\n95%\t1.96\n99%\t2.58\n99.9%\t3.29")
}

plots_sampleSize <- function(result = .sampleSize()) {
	result <- "Relevant output is in the Summary tab."
	return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
}

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
        selected = state_singlevar("ctl_dist", ctl_dist), multiple = FALSE),

      conditionalPanel(condition = "input.ctl_dist == 'runif'",
        div(class="row-fluid",
          div(class="span6",
            numericInput("ctl_unif_min", "Min:", value = 0, min = -10, max = 0, step = 1)
          ),
          div(class="span6",
            numericInput("ctl_unif_max", "Max:", value = 1, min = 1, max = 10, step = 1)
          )
        )
      ),
      conditionalPanel(condition = "input.ctl_dist == 'rnorm'",
        div(class="row-fluid",
          div(class="span6",
            numericInput("ctl_norm_mean", "Mean:", value = 0)
          ),
          div(class="span6",
            numericInput("ctl_norm_sd", "SD:", value = 1)
          )
        )
      ),
      conditionalPanel(condition = "input.ctl_dist == 'expo'",
        numericInput("ctl_expo_rate", "Rate:", value = 1, min = 1, step = 1)
      ),
      conditionalPanel(condition = "input.ctl_dist == 'binom'",
        div(class="row-fluid",
          div(class="span6",
            numericInput("ctl_binom_size", "Size:", value = 1, min = 1, max = 100, step = 1)
          ),
          div(class="span6",
            numericInput("ctl_binom_prob", "Prob:", value = 0.15, min = 0.01, max = 1, step = .05)
          )
        )
      ),

      div(class="row-fluid",
          div(class="span6",
            numericInput("ctl_n", "Sample size:",  value = 50, min = 2, max = 500, step = 1)
          ),
          div(class="span6",
            numericInput("ctl_m", "# of samples:",  value = 100, min = 2, max = 500, step = 1)
          )
      ),
      radioButtons("ctl_stat", label = "", choices = ctl_stat, selected = "Mean"),
		  actionButton("ctl_resample", "Resample")
		),
# 	 	helpAndReport('Central Limit Theorem','ctl',inclRmd("../quant/tools/help/ctl.Rmd"))
		helpModal('Central Limit Theorm','ctlHelp',inclRmd("../quant/tools/help/ctl.Rmd"))
 	)
})

output$ctl <- renderUI({
	# for input-output
  statTabPanel("Random", "Central Limit Theorem", ".ctl", "ctl")
})

.ctl<- reactive({

  if(is.null(input$ctl_dist)) return()
  validate(
    need(try(input$ctl_n > 1), message = "Please choose a sample size larger than 2."),
    need(try(input$ctl_m > 1), message = "Please choose 2 or more samples.")
  )

  # creating a dependency to a new set of draw is generated every time the button is pressed
  input$ctl_resample
	ctl(input$ctl_dist, input$ctl_n, input$ctl_m, input$ctl_stat)
})

 observe({
  if(is.null(input$ctlReport) || input$ctlReport == 0) return()
  isolate({
		inp <- list(input$ctl_dist, input$ctl_n, input$ctl_m, input$ctl_stat)
		updateReport(inp,"ctl")
  })
})

ctl <- function(ctl_dist, ctl_n, ctl_m, ctl_stat) {

  n <- ctl_n; m <- ctl_m; dist <- ctl_dist
  if(ctl_dist == "runif") {
    data <- matrix(runif(n*m, min=input$ctl_unif_min, max=input$ctl_unif_max), n, m)
  } else if (ctl_dist == "rnorm") {
    data <- matrix(rnorm(n*m, mean = input$ctl_norm_mean, sd = input$ctl_norm_sd), n, m)
  } else if (ctl_dist == "expo") {
    data <- matrix(rexp(n*m, rate = input$ctl_expo_rate), n, m)
  } else if (ctl_dist == "binom") {
    data <- matrix(rbinom(n*m, size = input$ctl_binom_size, prob=input$ctl_binom_prob), n, m)
  }
  data
}

summary_ctl <- function(result = .ctl()) {
    cat("See the Plots tab for output")
#    withMathJax("Testing math $\alpha$")
}

# getting back to the page in progress when developing
# observe( updateTabsetPanel(session, "nav_radiant", selected = "Central Limit Theorem") )

plots_ctl <- function(result = .ctl()) {

  ctl_stat <- input$ctl_stat
  if(ctl_stat == "Sum") {
    sstat <- data.frame("Sum" = colSums(result))
  } else {
    sstat <- data.frame("Mean" = colMeans(result))
  }

  data1 <- data.frame("Sample_1" = result[,1])
  datam <- data.frame("Sample_m" = result[,dim(result)[2]])

  bw <- diff(range(sstat, na.rm = TRUE)) / 10
  bwd1 <- diff(range(data1, na.rm = TRUE)) / 10
  bwdm <- diff(range(datam, na.rm = TRUE)) / 10

#   x <- stat_function(fun = dnorm, args = list(mean = 3, sd = .5, color = "blue"))
#   plots[[2]] <- ggplot(data, aes_string(x="Sample")) + geom_density(alpha=.3, fill = "green") + x

  plots <- list()
  plots[[1]] <- ggplot(data1, aes_string(x="Sample_1")) + geom_histogram(binwidth = bwd1)
  plots[[2]] <- ggplot(datam, aes_string(x="Sample_m")) + geom_histogram(binwidth = bwdm)
#   plots[[2]] <- ggplot(data, aes_string(x="Sample")) + geom_density(alpha=.3, fill = "green") +
#     stat_function(fun = dnorm, args = list(mean = mean(data[,1]), sd = sd(data[,1])), color = "blue") +
#     labs(y = "") + theme(axis.text.y = element_blank())
  plots[[3]] <- ggplot(sstat, aes_string(x=ctl_stat)) + geom_histogram(binwidth = bw)
  plots[[4]] <- ggplot(sstat, aes_string(x=ctl_stat)) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(sstat[,1]), sd = sd(sstat[,1])), color = "blue") +
      labs(y = "") + theme(axis.text.y = element_blank())

  do.call(grid.arrange, c(plots, list(ncol = min(2,length(plots)))))
}
