# alternative hypothesis options
base_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

################################
# Single proportion
###############################
output$uiSp_var <- renderUI({
  isFct <- "factor" == getdata_class()
  vars <- varnames()[isFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "sp_var", label = "Variable (select one):", choices = vars,
  	selected = state_singlevar("sp_var",vars), multiple = FALSE)
})

output$ui_singleProp <- renderUI({
  list(
    wellPanel(
      uiOutput("uiSp_var"),
      conditionalPanel(condition = "input.tabs_singleProp == 'Summary'",
	      selectInput(inputId = "sp_alternative", label = "Alternative hypothesis", choices = base_alt,
	      	selected = state_init_list("sp_alternative","two.sided", base_alt)),
        sliderInput('sp_sigLevel',"Significance level:", min = 0.85, max = 0.99,
      		value = state_init('sp_sigLevel',.95), step = 0.01),
    	  numericInput("sp_compValue", "Comparison value:", state_init('sp_compValue', 0.5), min = 0.01, max = 0.99, step = 0.01)
      )
    ),
	 	helpAndReport('Single proportion','singleProp',inclMD("../quant/tools/help/singleProp.md"))
  )
})

output$singleProp <- renderUI({
  # create inputs and outputs - function in radiant.R
  statTabPanel("Base","Single proportion",".singleProp","singleProp")
})

.singleProp <- reactive({

  ret_text <- "This analysis requires a variable of type factor with two levels.\nPlease select another dataset."
  if(is.null(input$sp_var)) return(ret_text)

  singleProp(input$dataset, input$sp_var, input$sp_compValue, input$sp_alternative, input$sp_sigLevel)
})

observe({
  if(is.null(input$singlePropReport) || input$singlePropReport == 0) return()
  isolate({
    inp <- list(input$dataset, input$sp_var, input$sp_compValue,
                input$sp_alternative, input$sp_sigLevel)
    updateReport(inp,"singleProp")
  })
})

singleProp <- function(dataset, sp_var, sp_compValue = 0.5, sp_alternative = 'two.sided',
                       sp_sigLevel = .95) {

  dat <- values[[dataset]][,sp_var]
	lev <- levels(dat)
	if(length(lev) >2) return("The selected variable has more than two levels.\nTry another variable or a cross-tab.")
	prop <- sum(dat == rev(lev)[1])
	result <- prop.test(prop, n = length(dat), p = sp_compValue, alternative = sp_alternative,
            conf.level = sp_sigLevel, correct = FALSE)
  result$data <- data.frame(dat)
  names(result$data) <- sp_var
  result$data.name <- sp_var
  result
}

summary_singleProp <- function(result = .singleProp()) {
  result
}

plots_singleProp <- function(result = .singleProp()) {

	var <- result$data.name
  dat <- na.omit(result$data)

 	p <- ggplot(dat, aes_string(x = var, fill = var)) + geom_histogram(alpha=.7) +
    ggtitle(paste("Single proportion:", var))
	print(p)
}

###############################
# Cross-tabs
###############################
output$uiCt_var1 <- renderUI({
	isFct <- "factor" == getdata_class()
  vars <- varnames()[isFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "ct_var1", label = "Select a grouping factor:", choices = vars,
  	# selected = names(vars[vars == values$ct_var1]), multiple = FALSE)
  	selected = state_singlevar("ct_var1",vars), multiple = FALSE)
})

output$uiCt_var2 <- renderUI({
	if(input$ct_var1 %>% not_available) return()
	isFct <- "factor" == getdata_class()
  vars <- varnames()[isFct]
	# if(!input$ct_var1 %in% vars) return()
	# if(is.null(inChecker(input$ct_var1))) return()
	vars <- vars[-which(vars == input$ct_var1)]
  if(length(vars) == 0) return()
  selectInput(inputId = "ct_var2", label = "Select a factor:", choices = vars,
  	selected = state_singlevar("ct_var2",vars), multiple = FALSE)
})

output$ui_crosstab <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiCt_var1"),
	    uiOutput("uiCt_var2"),
		  checkboxInput("ct_std_residuals", label = "Deviation (standarized)",
	     	value = state_init('ct_std_residuals',FALSE)),
		  checkboxInput("ct_deviation", label = "Deviation (percentage)",
	     	value = state_init('ct_deviation',FALSE)),
		  checkboxInput("ct_expected", label = "Expected values",
	     	value = state_init('ct_expected',FALSE)),
	    conditionalPanel(condition = "input.tabs_crosstab == 'Summary'",
			  checkboxInput("ct_contrib", label = "Contribution to chisquare value",
	     	value = state_init('ct_contrib',FALSE)))
		),
	 	helpAndReport('Cross-tabs','crosstab',inclMD("../quant/tools/help/crossTabs.md"))
  )
})

ct_plotWidth <- function() {
	result <- .crosstab()
	ifelse(is.list(result), return(result$plotWidth), return(650))
}

ct_plotHeight <- function() {
	result <- .crosstab()
	ifelse(is.list(result), return(result$plotHeight), return(650))
}

output$crosstab <- renderUI({
	# for input-output
  statTabPanel("Base", "Cross-tabs",".crosstab","crosstab", "ct_plotWidth", "ct_plotHeight")
})

.crosstab <- reactive({
	ret_text <- "This analysis requires variables of type factor.\nPlease select another dataset."
 # 	if(is.null(input$ct_var1) || is.null(input$ct_var2)) return(ret_text)
	# if(is.null(inChecker(c(input$ct_var1, input$ct_var2)))) return(ret_text)
	if(input$ct_var2 %>% not_available) return(ret_text)
	crosstab(input$dataset, input$ct_var1, input$ct_var2, input$ct_expected, input$ct_deviation,
		input$ct_std_residuals, input$ct_contrib)
})

observe({
  if(is.null(input$crosstabReport) || input$crosstabReport == 0) return()
  isolate({

		inp <- list(input$dataset, input$ct_var1, input$ct_var2, input$ct_expected, input$ct_deviation,
			input$ct_std_residuals, input$ct_contrib)

		updateReport(inp,"crosstab", round(7 * ct_plotWidth()/650,2), round(7 * ct_plotHeight()/650,2))
  })
})

crosstab <- function(dataset, ct_var1, ct_var2, ct_expected, ct_deviation, ct_std_residuals, ct_contrib) {

  dat <- na.omit( values[[dataset]][,c(ct_var1,ct_var2)] )

	dnn = c(paste("Group(",ct_var1,")",sep = ""), paste("Variable(",ct_var2,")",sep = ""))
	tab <- table(dat[,ct_var1], dat[,ct_var2], dnn = dnn)
	# cst <- sshh(chisq.test(tab, correct = FALSE))
	cst <- chisq.test(tab, correct = FALSE)

	# adding the % deviation table
	o <- cst$observed
	e <- cst$expected
	cst$deviation <- (o-e) / e

	nrPlot <- 1 + sum(c(ct_expected,ct_deviation, ct_std_residuals))

	cinp <- list()
	cinp$dataset <- dataset
	cinp$ct_var1 <- ct_var1
	cinp$ct_var2 <- ct_var2
	cinp$ct_expected <- ct_expected
	cinp$ct_deviation <- ct_deviation
	cinp$ct_std_residuals <- ct_std_residuals
	cinp$ct_contrib <- ct_contrib

	list(cst = cst, table = tab, plotWidth = 650, plotHeight = 400 * nrPlot, cinp = cinp)
}

summary_crosstab <- function(result = .crosstab()) {
	cat("Observed values:\n")
	print(result$cst$observed)

	cinp <- result$cinp

	if(cinp$ct_std_residuals) {
		cat("\nDeviation (standardized):\n")
		print(round(result$cst$residuals, 2)) 	# these seem to be the correct std.residuals
	}
	if(cinp$ct_deviation) {
		cat("\nDeviation (percentage):\n")
		print(round(result$cst$deviation, 2)) 	# % deviation
	}
	if(cinp$ct_expected) {
		cat("\nExpected values:\n")
		print(round(result$cst$expected,2))
		# print(result$cst$expected, digits = 2)
	}
	if(cinp$ct_contrib) {
		cat("\nContribution to chisquare value:\n")
		print((result$cst$observed - result$cst$expected)^2 / result$cst$expected, digits = 2)
		# print(round((result$cst$observed - result$cst$expected)^2 / result$cst$expected), 2)
	}
	# if(cinp$ct_cellperc) {
	# 	cat("\nCell percentages:\n")
	# 	print(prop.table(result$table), digits = 2)  	# cell percentages
	# }
	# if(cinp$ct_rowperc) {
	# 	cat("\nRow percentages:\n")
	# 	print(prop.table(result$table, 1), digits = 2) # row percentages
	# }
	# if(cinp$ct_colperc) {
	# 	cat("\nColumn percentages:\n")
	# 	print(prop.table(result$table, 2), digits = 2) # column percentages
	# }

	print(result$cst)
	# cat(paste("\n",sprintf("%.1f",100 * (sum(result$cst$expected < 5) / length(result$cst$expected))),"% of cells have expected values below 5\n\n"), sep = "")
	cat(paste(sprintf("%.1f",100 * (sum(result$cst$expected < 5) / length(result$cst$expected))),"% of cells have expected values below 5\n\n"), sep = "")
}

plots_crosstab <- function(result = .crosstab()) {

	cinp <- result$cinp

  dat <- na.omit( values[[cinp$dataset]][,c(cinp$ct_var1,cinp$ct_var2)] )
	# dat <- na.omit( getdata()[,c(cinp$ct_var1,cinp$ct_var2)] )
	plots <- list()

  ##### Should probably change the name of this function
  ##### if you are switching to tidyr
	meltTable <- function(tab) {
		tab <- data.frame(tab, check.names = FALSE)
		lab <- data.frame(rownames(tab))
		names(lab) <- "rnames"
		melt(cbind(lab,tab))
    # gather seems to be the equivalent function to melt in tidyr
# 		gather(cbind(lab,tab))
	}

	if(cinp$ct_std_residuals) {

		tab <- meltTable(result$cst$residuals)
		colnames(tab)[c(2,3)] <- c(cinp$ct_var1, cinp$ct_var2)
		plots[['residuals']] <- ggplot(tab, aes_string(x = cinp$ct_var1, y = "value", fill = cinp$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) +
     					geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
     					geom_text(data = NULL, x = 1, y = 2.11, label = "95%") +
     					geom_text(data = NULL, x = 1, y = 1.49, label = "90%") +
         			labs(list(title = paste("Deviation (standardized) for ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""), x = cinp$ct_var1))
	}

	if(cinp$ct_deviation) {

		tab <- meltTable(result$cst$deviation)
		colnames(tab)[c(2,3)] <- c(cinp$ct_var1, cinp$ct_var2)
		plots[['deviation']] <- ggplot(tab, aes_string(x = cinp$ct_var1, y = "value", fill = cinp$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) + ylim(-1,1) +
         			labs(list(title = paste("Deviation (percentage) for ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""), x = cinp$ct_var1))
	}

	if(cinp$ct_expected) {

  	tab <- meltTable(result$cst$expected)
		tab$rnames <- factor(tab$rnames,levels=levels(dat[,1]))
		tab$variable <- factor(tab$variable,levels=levels(dat[,2]))

# 		plots[['expected']] <- ggplot(tab, aes_string(x = 'rnames', y = "value", fill = "variable")) +
		plots[['expected']] <- ggplot(tab, aes_string(x = 'rnames', y = "value", fill = "variable")) +
#          			geom_bar(stat="identity", position = "dodge", alpha = .3) +
         			geom_bar(stat="identity", position = "fill", alpha = .7) +
#          			geom_bar(position = "fill", alpha = .3) +
         			labs(list(title = paste("Expected values for ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""),
							x = "", y = "", fill = cinp$ct_var2))
	}

	plots[['stacked']] <- ggplot(dat, aes_string(x = cinp$ct_var1, fill = cinp$ct_var2)) + geom_bar(position = "fill", alpha=.7) +
		labs(list(title = paste("Observed values for ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""),
				x = "", y = "", fill = cinp$ct_var2))

	# plots[['observed']] <- ggplot(dat, aes_string(x = cinp$ct_var1, fill = cinp$ct_var2)) + geom_histogram(position = "dodge", alpha=.3) +
	# 	labs(list(title = paste("Crosstab of ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""),
	# 			x = '', y = "Count", fill = cinp$ct_var2))

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}
