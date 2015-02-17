###############################
# Single proportion - ui
###############################

# alternative hypothesis options
sp_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
sp_plots <- c("Histogram" = "hist", "Simulate" = "simulate")

# list of function arguments
sp_args <- as.list(formals(single_prop))

# list of function inputs selected by user
sp_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(sp_args))
    sp_args[[i]] <- input[[i]]
  sp_args
})

output$ui_sp_var <- renderUI({
  # isFct <- "factor" == getdata_class()
  # vars <- varnames()[isFct]
  # vars <- two_level_vars()
  vars <- groupable_vars()
  selectInput(inputId = "sp_var", label = "Variable (select one):",
              choices = vars,
              selected = state_single("sp_var",vars), multiple = FALSE)
})

output$ui_sp_levels <- renderUI({
  if(input$sp_var %>% not_available)
    levs <- c()
  else
    levs <- getdata()[1,input$sp_var] %>% as.factor %>% levels

  selectInput(inputId = "sp_levels", label = "Choose level:",
              choices = levs,
              selected = state_single("sp_levels",levs), multiple = FALSE)
})

output$ui_single_prop <- renderUI({
  tagList(
  	wellPanel(
      conditionalPanel(condition = "input.tabs_single_prop == 'Plot'",
        selectizeInput(inputId = "sp_plots", label = "Select plots:",
                choices = sp_plots,
                selected = state_single("sp_plots", sp_plots, sp_args$sp_plots),
                multiple = TRUE,
                options = list(plugins = list('remove_button', 'drag_drop')))),
 	   	uiOutput("ui_sp_var"),
      uiOutput("ui_sp_levels"),
   	  selectInput(inputId = "sp_alternative", label = "Alternative hypothesis:",
  	  	choices = sp_alt,
        selected = state_single("sp_alternative", sp_alt, sp_args$sp_alternative),
  	  	multiple = FALSE),
    	sliderInput('sp_sig_level',"Significance level:", min = 0.85, max = 0.99,
    		value = state_init('sp_sig_level', sp_args$sp_sig_level), step = 0.01),
      numericInput("sp_comp_value", "Comparison value:",
                   state_init('sp_comp_value', sp_args$sp_comp_value),
                   min = 0.01, max = 0.99, step = 0.01)),

    help_and_report(modal_title = 'Single proportion',
  	                fun_name = 'single_prop',
                    help_file = inclMD("../quant/tools/help/single_prop.md"))
  )
})

sp_plot_height <- function() {
  result <- .single_prop()
  ifelse(!"character" %in% class(result), result$plot_height, 400)
}

# output is called from the main radiant ui.R
output$single_prop <- renderUI({

		register_print_output("summary_single_prop", ".single_prop")
		register_plot_output("plot_single_prop", ".single_prop",
                         height_fun = "sp_plot_height")

		# two separate tabs
		sp_output_panels <- tabsetPanel(
	    id = "tabs_single_prop",
	    tabPanel("Summary", verbatimTextOutput("summary_single_prop")),
	    tabPanel("Plot", plotOutput("plot_single_prop", height = "100%"))
	  )

		statTabPanel2(menu = "Base",
		              tool = "Single proportion",
		              tool_ui = "ui_single_prop",
		             	output_panels = sp_output_panels)
})

.single_prop <- reactive({

  if(input$sp_var %>% not_available)
    return("This analysis requires a variable of type factor with two levels.\nPlease select another dataset.\n\n" %>% suggest_data("facebook"))

  if(input$sp_comp_value %>% { is.na(.) |
     is_weakly_greater_than(.,1) |
     is_weakly_less_than(.,0) } )
  	return("Please choose a comparison value between 0 and 1")

	do.call(single_prop, sp_inputs())
})


observe({
  if(input$single_prop_report %>% not_pressed) return()
  isolate({
		update_report(inp = sp_inputs() %>% clean_args, fun_name = "single_prop",
		              outputs = c("summary", "plot"))
  })
})
