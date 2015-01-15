###############################
# Single mean
###############################
single_mean <- function(dataset, sm_var,
                        dataset_filter = "",
                        sm_comp_value = 0,
                        sm_alternative = "two.sided",
                        sm_sig_level = .95) {

	if(exists("values"))
		dat <- select_(values[[dataset]], sm_var)
		# if a data_filter has been defined
		# dat <- filter(dat, values[[dataset_filter]])
	else
		dat <- select_(get(dataset), sm_var)

	t.test(dat, mu = sm_comp_value, alternative = sm_alternative,
	       conf.level = sm_sig_level) %>% tidy -> res

  # as.list(environment())
  environment() %>% as.list
}

summary_single_mean <- function(result = .single_mean()) {

	cat("Data     :", result$dataset, "\n")
	# cat("Filter   :", result$xtra$filter, "\n")
	cat("Variable :", result$sm_var, "\n")

	if (result$sm_alternative == "two.sided") {
		hyp_symbol <- "not equal to"
	} else if (result$sm_alternative == "less") {
		hyp_symbol <- "<"
	} else {
		hyp_symbol <- ">"
	}

	cat("Null hyp.: the mean of", result$sm_var, "=", result$sm_comp_value, "\n")
	cat("Alt. hyp.: the mean of", result$sm_var, "is", hyp_symbol,
	    result$sm_comp_value, "\n\n")

	# determine lower and upper % for ci
	{100 * (1-result$sm_sig_level)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%") -> ci_perc

	result$res$n <- nrow(result$dat)
	result$res$sd <- sd(result$dat[,result$sm_var])
	res <- round(result$res, 3) 	# restrict to 3 decimal places
	names(res)[1:6] <- c("mean","t.value","p.value","df", ci_perc[1], ci_perc[2])
	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

plots_single_mean <- function(result = .single_mean()) {

	bw <- diff(range(result$dat, na.rm = TRUE)) / 12
	res <- result$res
	p <- ggplot(result$dat, aes_string(x=result$sm_var)) +
		geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
		geom_vline(xintercept = result$sm_comp_value, color = 'red',
		           linetype = 'longdash', size = 1) +
		geom_vline(xintercept = result$res$estimate, color = 'black',
		           linetype = 'solid', size = 1) +
		geom_vline(xintercept = c(result$res$conf.low, result$res$conf.high),
		           color = 'black', linetype = 'longdash', size = .5)
	print(p)
}


###############################
# Single mean - ui
###############################

# alternative hypothesis options
sm_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

# list of function arguments
sm_args <- as.list(formals(single_mean))

# list of function inputs selected by user
sm_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(sm_args))
    sm_args[[i]] <- input[[i]]
  sm_args
})

output$ui_sm_var <- renderUI({
  isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "sm_var", label = "Variable (select one):",
              choices = vars,
              selected = state_singlevar("sm_var",vars), multiple = FALSE)
})

output$ui_single_mean <- renderUI({
  tagList(
  	wellPanel(
 	   	uiOutput("ui_sm_var"),
  	  selectInput(inputId = "sm_alternative", label = "Alternative hypothesis:",
  	  	choices = sm_alt,
        selected = state_init_list("sm_alternative",sm_args$sm_alternative, sm_alt),
  	  	multiple = FALSE),
    	sliderInput('sm_sig_level',"Significance level:", min = 0.85, max = 0.99,
    		value = state_init('sm_sig_level',sm_args$sm_sig_level), step = 0.01),
    	numericInput("sm_comp_value", "Comparison value:",
    	             state_init('sm_comp_value',sm_args$sm_comp_value))
  	),
  	help_and_report(modal_title = 'Single mean',
  	                fun_name = 'single_mean',
  	                help_file = inclMD("../quant/tools/help/single_mean.md")
    )
 	)
})

# output is called from the main radiant ui.R
output$single_mean <- renderUI({

		register_print_output("summary_single_mean", ".single_mean")
		register_plot_output("plots_single_mean", ".single_mean")

		# two separate tabs
		sm_output_panels <- tabsetPanel(
	    id = "tabs_single_mean",
	    tabPanel("Summary", verbatimTextOutput("summary_single_mean")),
	    tabPanel("Plots", plotOutput("plots_single_mean", height = "100%"))
	  )

		# one output with components stacked
		# sm_output_panels <- tagList(
	  #    tabPanel("Summary", verbatimTextOutput("summary_single_mean")),
	  #    tabPanel("Plots", plotOutput("plots_single_mean", height = "100%"))
	  # )

		statTabPanel2(menu = "Base",
		              tool = "Single mean",
		              tool_ui = "ui_single_mean",
		             	output_panels = sm_output_panels)

		# add "data = NULL" if the app doesn't doesn't use data
})

.single_mean <- reactive({

  if(input$sm_var %>% not_available)
    return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")

  if(input$sm_comp_value %>% is.na)
  	return("Please choose a comparison value.")

	do.call(single_mean, sm_inputs())
})


observe({
  if(input$single_mean_report %>% not_pressed) return()
  isolate({
		update_report(inp = sm_inputs(), fun_name = "single_mean",
		              outputs = c("summary_single_mean", "plots_single_mean"))
  })
})
