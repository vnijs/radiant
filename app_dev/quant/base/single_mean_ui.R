# alternative hypothesis options
base_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

# list of inputs
# base_sm_variables <- c("datasets", "sm_var", "sm_comp_value", "sm_alternative", "sm_sig_level")
c("datasets", "sm_var", "sm_comp_value", "sm_alternative", "sm_sig_level") %>%
	setNames(as.list(.),.) -> base_sm_list


###############################
# Single mean
###############################

# check all the variables and function names, use all underscores now
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
  	  	choices = base_alt,
  	  	selected = state_init_list("sm_alternative","two.sided", base_alt),
  	  	multiple = FALSE),
    	sliderInput('sm_sig_level',"Significance level:", min = 0.85, max = 0.99,
    		value = state_init('sm_sig_level',.95), step = 0.01),
    	numericInput("sm_comp_value", "Comparison value:",
    	             state_init('sm_comp_value',0.0))
  	),
  	help_and_report(modal_title = 'Single mean',
  	                fun_name = 'single_mean',
  	                help_file = inclMD("../quant/tools/help/single_mean.md")
    )
 	)
})

# this is the name of the output that will be called from the main
# radiant ui file
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
	 #  )

		statTabPanel2(menu = "Base",
		              tool = "Single mean",
		              tool_ui = "ui_single_mean",
		             	output_panels = sm_output_panels)

		# add "data = NULL" if the app doesn't doesn't use data
})

.single_mean <- reactive({

#
#
# yeugh. clean that up into one function
#
#
  rtext <- "This analysis requires a variable of type numeric or interval.\nPlease select another database"
 	if(is.null(input$sm_var)) return(rtext)
	if(is.null(inChecker(c(input$sm_var)))) return(rtext)

  if(input$sm_comp_value %>% is.na)
  	return("Please choose a comparison value.")

  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(base_sm_list))
  	base_sm_list[[i]] <- input[[i]]
	do.call(single_mean, base_sm_list)
})


observe({

	#
	#
	# yeugh. need to button-check function
	#
	#

  if(is.null(input$single_mean_report) || input$single_mean_report == 0) return()

  isolate({
  	# loop needed because reactive values don't allow single bracket indexing
  	for(i in names(base_sm_list))
  		base_sm_list[[i]] <- input[[i]]
		update_report(inp = base_sm_list, fun_name = "single_mean",
		              outputs = c("summary_single_mean", "plots_single_mean"))
  })
})
