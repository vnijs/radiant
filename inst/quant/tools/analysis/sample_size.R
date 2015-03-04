###############################
# Sample size
###############################
rnd_mean <- c("Mean" = "mean", "Proportion" = "proportion")
rnd_pop_correction <- c("Yes" = "yes", "No" = "no")

output$ui_sample_size <- renderUI({
  list(
  	wellPanel(
      # radioButtons(inputId = "rnd_mean", label = "", rnd_mean,
		  radioButtons(inputId = "rnd_mean", label = NULL, choices = rnd_mean,
	  	  selected = state_init("rnd_mean", "mean"),
        inline = TRUE),
		  conditionalPanel(condition = "input.rnd_mean == 'mean'",
		    numericInput("rnd_mean_err", "Acceptable Error (e.g., $10):", min = 0,
		  		value = state_init("rnd_mean_err", 2), step = .1),
		    numericInput("rnd_mean_s", "Sample std. deviation:", min = 0,
		  		value = state_init("rnd_mean_s", 10), step = .1)
	  	),
		  conditionalPanel(condition = "input.rnd_mean != 'mean'",
		  	numericInput("rnd_prop_err", "Acceptable Error (e.g., .03):", min = 0,
          max = 1, value = state_init("rnd_prop_err", .1), step = .01),
		    numericInput("rnd_prop_p", "Sample proportion:", min = 0, max = 1,
		  		value = state_init("rnd_prop_p", .5), step = .05)
	  	),
	    numericInput("rnd_z", "Confidence level (z-value):", min = 0,
	  		value = state_init("rnd_z", 1.96), step = .1),
	    numericInput("rnd_incidence", "Incidence rate:", min = 0, max = 1,
	  		value = state_init("rnd_incidence", 1), step = .05),
	    numericInput("rnd_response", "Response rate:", min = 0, max = 1,
	  		value = state_init("rnd_response", 1), step = .05),
      radioButtons(inputId = "rnd_pop_correction",
        choices = rnd_pop_correction,
        label = "Correct for population size:",
        selected = state_init("rnd_pop_correction", "no"),
        inline = TRUE),
		  conditionalPanel(condition = "input.rnd_pop_correction == 'yes'",
		    numericInput("rnd_pop_size", "Population size:", min = 1,
		  		value = state_init("rnd_pop_size", 10^6), step = 1000))
		),
	 	helpAndReport('Sample size','sample_size',inclRmd("../quant/tools/help/sample_size.Rmd"))
 	)
})

output$sample_size <- renderUI({
	# for input-output
  statTabPanel("Random", "Sample size", ".sample_size", "sample_size")
})

.sample_size <- reactive({
	if(is.null(input$rnd_mean)) return("")
	sample_size(input$rnd_mean, input$rnd_mean_err, input$rnd_mean_s, input$rnd_prop_err,
			input$rnd_prop_p, input$rnd_z, input$rnd_incidence, input$rnd_response, input$rnd_pop_correction,
			input$rnd_pop_size)
})

observe({
  if(not_pressed(input$sample_sizeReport)) return()
  isolate({
		inp <- list(input$rnd_mean, input$rnd_mean_err, input$rnd_mean_s, input$rnd_prop_err,
			input$rnd_prop_p, input$rnd_z, input$rnd_incidence, input$rnd_response, input$rnd_pop_correction,
			input$rnd_pop_size)
		updateReport(inp,"sample_size")
  })
})

sample_size <- function(rnd_mean, rnd_mean_err, rnd_mean_s, rnd_prop_err, rnd_prop_p, rnd_z, rnd_incidence,
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

summary_sample_size <- function(result = .sample_size()) {
	cat("Required sample size:", result$n)
	cat("\nRequired contact attempts:", result$n / result$rnd_incidence / result$rnd_response)
	cat("\n\nChoose a Z-value as follows:\n")

  zvals <- c(.90, .95, .99, .9973, .999)
  for(z in zvals)
    cat(paste0(100*z,"%\t"),-qnorm((1-z)/2) %>% round(2),"\n")

	# cat("90%\t1.64\n95%\t1.96\n99%\t2.58\n99.9%\t3.29")
}

plots_sample_size <- function(result = .sample_size()) {
	result <- "Relevant output is in the Summary tab."
	return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
}
