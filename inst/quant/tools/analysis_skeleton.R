###############################
# tool
###############################
output$uiTool_var <- renderUI({
	# variable selection
  vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "tool_var", label = "Variable (select one):", choices = vars, 
  	selected = state_singlevar("tool_var",vars), multiple = FALSE)
})

output$ui_tool <- renderUI({
	# for ui
  list(
  	wellPanel(
  		uiOutput("uiTool_var")
  	),
 		helpAndReport('The tool','tool',includeHTML("tools/help/tool.html"))
 	)
})

output$tool <- renderUI({
	# for input-output
  statTabPanel("Menu", "The tool", ".tool", "tool", widthFun = "plotWidth", heightFun = "plotHeight")
  # can insert custom functions for plotWidth and plotHeight
})

.tool <- reactive({
	# reactive that calls the function for main analysis
	# . used to indicate this is an 'internal' function
	tool()
})

tool <- function(datasets, ...) {
	# function for main analysis

  ...

  result <- list()

  ... 

  return(result)
}
})

# Generate output for the summary tab
summary_tool <- function(result = .tool()) {

}

# Generate output for the plots tab
plots_tool <- function(result = .tool()) {

}