# alternative hypothesis options
base_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

###############################
# Single mean
###############################
output$uiSm_var <- renderUI({
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "sm_var", label = "Variable (select one):", choices = vars, 
  	selected = state_singlevar("sm_var",vars), multiple = FALSE)
})

output$ui_singleMean <- renderUI({
  list(
  	wellPanel(
 	   	uiOutput("uiSm_var"),
  	  selectInput(inputId = "sm_alternative", label = "Alternative hypothesis:", 
  	  	choices = base_alt, selected = state_init_list("sm_alternative","two.sided", base_alt), multiple = FALSE),
    	sliderInput('sm_sigLevel',"Significance level:", min = 0.85, max = 0.99, 
    		value = state_init('sm_sigLevel',.95), step = 0.01),
    	numericInput("sm_compValue", "Comparison value:", state_init('sm_compValue',0.0))
  	),
		helpAndReport('Single mean','singleMean',inclMD("tools/help/singleMean.md"))
 	)
})

output$singleMean <- renderUI({
	# create inputs and outputs - function in radiant.R
  statTabPanel("Base","Single mean",".singleMean","singleMean")
})

.singleMean <- reactive({

	ret_text <- "This analysis requires a variable of type numeric or interval.\nPlease select another dataset."
	if(is.null(input$sm_var)) return(ret_text)
	# if(is.null(inChecker(c(input$sm_var)))) return(ret_text)

	singleMean(input$datasets, input$sm_var, input$sm_compValue, input$sm_alternative, input$sm_sigLevel)
})

observe({
  if(is.null(input$singleMeanReport) || input$singleMeanReport == 0) return()
  isolate({
		inp <- list(input$datasets, input$sm_var, input$sm_compValue, 
			input$sm_alternative, input$sm_sigLevel)
		updateReport(inp,"singleMean")
  })
})

singleMean <- function(datasets, sm_var, sm_compValue = 0, sm_alternative = 'two.sided', 
	sm_sigLevel = .95) {

	dat <- values[[datasets]][,sm_var]
	result <- t.test(dat, mu = sm_compValue, alternative = sm_alternative, 
		conf.level = sm_sigLevel)
	result$data <- data.frame(dat)
	names(result$data) <- sm_var
	result$data.name <- sm_var
	result
}

summary_singleMean <- function(result = .singleMean()) {
	result
}

plots_singleMean <- function(result = .singleMean()) {

	dat <- result$data
	bw <- diff(range(dat, na.rm = TRUE)) / 12

	p <- ggplot(dat, aes_string(x=result$data.name)) + 
		geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
		geom_vline(xintercept = c(result$null.value), color = 'red', linetype = 'longdash', size = 1) +
		geom_vline(xintercept = result$estimate, color = 'black', linetype = 'solid', size = 1) +
		geom_vline(xintercept = result$conf.int, color = 'black', linetype = 'longdash', size = .5)
	print(p)
}

###############################
# Compare means
###############################
output$uiCm_var1 <- renderUI({

	isNumOrFct <- "numeric" == getdata_class() | "integer" == getdata_class() | "factor" == getdata_class()
  vars <- varnames()[isNumOrFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "cm_var1", label = "Select a factor or numerical variable:", choices = vars, 
  	selected = state_singlevar("cm_var1",vars), multiple = FALSE)
})

output$uiCm_var2 <- renderUI({

  if(is.null(input$cm_var1)) return()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  if(length(vars) == 0) return()
 	if(input$cm_var1 %in% vars) {
 		# when cm_var1 is numeric comparison for multiple variables are possible
	 	vars <- vars[-which(vars == input$cm_var1)]
	  if(length(vars) == 0) return()
	  selectInput(inputId = "cm_var2", label = "Variables (select one or more):", choices = vars, 
	  	selected = state_multvar("cm_var2",vars), multiple = TRUE, selectize = FALSE)
	} else {
 		# when cm_var1 is not numeric then comparisons are across levels/groups
	  selectInput(inputId = "cm_var2", label = "Variables (select one):", choices = vars, 
	  	selected = state_singlevar("cm_var2",vars), multiple = FALSE)
	}
})

output$ui_compareMeans <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiCm_var1"),
	    uiOutput("uiCm_var2"),
	    conditionalPanel(condition = "input.tabs_compareMeans == 'Summary'",
	      selectInput(inputId = "cm_alternative", label = "Alternative hypothesis:", choices = base_alt, 
	      	selected = state_init_list("cm_alternative","two.sided", base_alt))
	    ),
	    conditionalPanel(condition = "input.tabs_compareMeans == 'Plots'",
			  checkboxInput('cm_jitter', 'Jitter', value = state_init("cm_jitter",FALSE))
			)
		),
  	helpAndReport('Compare means','compareMeans',inclMD("tools/help/compareMeans.md"))
  )
})

output$compareMeans <- renderUI({
  statTabPanel("Base","Compare means",".compareMeans", "compareMeans")
})

.compareMeans <- reactive({

	ret_text <- "This analysis requires variables of type factor, numeric or interval.\nPlease select another dataset."
	if(is.null(input$cm_var1)) return(ret_text)
	if(is.null(input$cm_var2)) return("Please select a numeric or interval variable")
	# if(is.null(inChecker(c(input$cm_var1, input$cm_var2)))) return(ret_text)

	compareMeans(input$datasets, input$cm_var1, input$cm_var2, input$cm_alternative, input$cm_jitter)
})

observe({
  if(is.null(input$compareMeansReport) || input$compareMeansReport == 0) return()
  isolate({
		inp <- list(input$datasets, input$cm_var1, input$cm_var2, input$cm_alternative, input$cm_jitter)
		updateReport(inp,"compareMeans")
  })
})

compareMeans <- function(datasets, var1, var2, cm_alternative, cm_jitter) {

	vars <- c(var1,var2)
	dat <- values[[datasets]][,vars]
	dat <- na.omit(dat)

	if(!is.factor(dat[,var1])) {
		cm_paired <- TRUE
		dat <- melt(dat)
		var1 <- colnames(dat)[1]
		var2 <- colnames(dat)[2]
	} else {
		cm_paired <- FALSE
		colnames(dat)[1] <- "variable"
	}

	if(cm_paired) {
		pwcomp <- with(dat,pairwise.t.test(get(var2), get('variable'), pool.sd = FALSE, 
			p.adj = "bonf", paired = TRUE, alternative = cm_alternative))
	} else {
		pwcomp <- with(dat,pairwise.t.test(get(var2), get('variable'), pool.sd = FALSE, 
			p.adj = "bonf", paired = FALSE, alternative = cm_alternative))
	}

	pwcomp$vars <- paste0(vars, collapse=", ")
	pwcomp$cm_alternative <- cm_alternative
	pwcomp$cm_jitter <- cm_jitter

	list("pwcomp" = pwcomp, "data" = data.frame(dat))
}


# Generate output for the summary tab
summary_compareMeans <- function(result = .compareMeans()) {

	cat("Pairwise comparisons using t-tests (bonferroni adjustment)\n")
	cat(paste0("Variables: ",result$pwcomp$vars,"\n\n"))
	# cat("\nMeans table:\n")
	means_tab <- ddply(result$data, c("variable"), colwise(mean))
	colnames(means_tab) <- c("","mean")
	print(means_tab, row.names = FALSE, right = FALSE)

	if(result$pwcomp$cm_alternative == "two.sided") {
		h.sym <- "not equal to" 
	} else if(result$pwcomp$cm_alternative == "less") {
		h.sym <- "<" 
	} else {
		h.sym <- ">" 
	}

	mod <- result[['pwcomp']]$p.value
	dvar <- dimnames(mod)
	var1 <- dvar[[1]] 
	var2 <- dvar[[2]] 

	res <- data.frame(matrix(ncol = 3, nrow = length(var1)*length(var1)/2))
	colnames(res) <- c("Alternative hyp.", "Null hyp.", "p-value")

	rnr <- 1
	for(i in var1) {
		for(j in var2) {
			if(is.na(mod[i,j])) next
			res[rnr, 'Alternative hyp.'] <- paste(i, h.sym, j,"     ") 
			res[rnr, 'Null hyp.'] <- paste(i, "=", j, "     ") 
			if(mod[i,j] < .001) { 
				pval = "< 0.001"
			} else {
				pval <- sprintf("%.3f", mod[i,j])
			}
			res[rnr, 'p-value'] <- pval
			rnr <- rnr + 1
		}
	}
	cat("\n")	
	print(res, row.names = FALSE, right = FALSE)
}

# Generate output for the plots tab
plots_compareMeans <- function(result = .compareMeans()) {

	dat <- result$data
	var1 <- colnames(dat)[1]
	var2 <- colnames(dat)[-1]

	plots <- list()
	# p <- ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) + geom_boxplot(alpha=.3, legend = FALSE) 
	p <- ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) + geom_boxplot(alpha=.3) 
	if(result$pwcomp$cm_jitter)	p <- p + geom_jitter() 
	plots[["Boxplot"]] <- p
	plots[["Density"]] <- ggplot(dat, aes_string(x=var2, fill=var1)) + geom_density(alpha=.3)

	do.call(grid.arrange, c(plots, list(ncol = 1)))
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
	if(is.null(input$ct_var1)) return()
	isFct <- "factor" == getdata_class()
  vars <- varnames()[isFct]
	# if(!input$ct_var1 %in% vars) return()
	# if(is.null(inChecker(input$ct_var1))) return()
	vars <- vars[-which(vars == input$ct_var1)]
  if(length(vars) == 0) return()
  selectInput(inputId = "ct_var2", label = "Select a factor:", choices = vars, 
  	# selected = names(vars[vars == values$ct_var2]), multiple = FALSE)
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
	 	helpAndReport('Cross-tabs','crosstab',inclMD("tools/help/crossTabs.md"))
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
 	if(is.null(input$ct_var1) || is.null(input$ct_var2)) return(ret_text)
	# if(is.null(inChecker(c(input$ct_var1, input$ct_var2)))) return(ret_text)
	crosstab(input$datasets, input$ct_var1, input$ct_var2, input$ct_expected, input$ct_deviation, 
		input$ct_std_residuals, input$ct_contrib) 
})

observe({
  if(is.null(input$crosstabReport) || input$crosstabReport == 0) return()
  isolate({

		inp <- list(input$datasets, input$ct_var1, input$ct_var2, input$ct_expected, input$ct_deviation, 
			input$ct_std_residuals, input$ct_contrib) 

		updateReport(inp,"crosstab", round(7 * ct_plotWidth()/650,2), round(7 * ct_plotHeight()/650,2))
  })
})

crosstab <- function(datasets, ct_var1, ct_var2, ct_expected, ct_deviation, ct_std_residuals, ct_contrib) {

  dat <- na.omit( values[[datasets]][,c(ct_var1,ct_var2)] )

	dnn = c(paste("Group(",ct_var1,")",sep = ""), paste("Variable(",ct_var2,")",sep = ""))
	tab <- table(dat[,ct_var1], dat[,ct_var2], dnn = dnn)
	cst <- suppressWarnings(suppressMessages( chisq.test(tab, correct = FALSE) ))

	# adding the % deviation table
	o <- cst$observed
	e <- cst$expected
	cst$deviation <- (o-e) / e

	nrPlot <- 1 + sum(c(ct_expected,ct_deviation, ct_std_residuals))

	cinp <- list()
	cinp$datasets <- datasets
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

  dat <- na.omit( values[[cinp$datasets]][,c(cinp$ct_var1,cinp$ct_var2)] )
	# dat <- na.omit( getdata()[,c(cinp$ct_var1,cinp$ct_var2)] )
	plots <- list()

	meltTable <- function(tab) {
		tab <- data.frame(tab)
		lab <- data.frame(rownames(tab))
		names(lab) <- "rnames"
		melt(cbind(lab,tab))
	}

	if(cinp$ct_std_residuals) {

		tab <- meltTable(result$cst$residuals)
		colnames(tab)[c(2,3)] <- c(cinp$ct_var1, cinp$ct_var2)
		plots[['residuals']] <- ggplot(tab, aes_string(x = cinp$ct_var1, y = "value", fill = cinp$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .3) +
     					geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
     					geom_text(data = NULL, x = 1, y = 2.11, label = "95%") +
     					geom_text(data = NULL, x = 1, y = 1.49, label = "90%") +
         			labs(list(title = paste("Deviation (standardized) for ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""), x = cinp$ct_var1))
	}

	if(cinp$ct_deviation) {

		tab <- meltTable(result$cst$deviation)
		colnames(tab)[c(2,3)] <- c(cinp$ct_var1, cinp$ct_var2)
		plots[['deviation']] <- ggplot(tab, aes_string(x = cinp$ct_var1, y = "value", fill = cinp$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .3) + ylim(-1,1) +
         			labs(list(title = paste("Deviation (percentage) for ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""), x = cinp$ct_var1))
	}

	if(cinp$ct_expected) {

		tab <- meltTable(result$cst$expected)
		tab$rnames <- factor(tab$rnames,levels=levels(dat[,1]))

		plots[['expected']] <- ggplot(tab, aes_string(x = 'rnames', y = "value", fill = "variable")) +
         			# geom_bar(stat="identity", position = "dodge", alpha = .3) +
         			geom_bar(position = "fill", alpha = .3) +
         			labs(list(title = paste("Expected values for ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""), 
							x = "", y = "", fill = cinp$ct_var2))
	}

	plots[['stacked']] <- ggplot(dat, aes_string(x = cinp$ct_var1, fill = cinp$ct_var2)) + geom_bar(position = "fill", alpha=.3) +
		labs(list(title = paste("Observed values for ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""), 
				x = "", y = "", fill = cinp$ct_var2))

	# plots[['observed']] <- ggplot(dat, aes_string(x = cinp$ct_var1, fill = cinp$ct_var2)) + geom_histogram(position = "dodge", alpha=.3) +
	# 	labs(list(title = paste("Crosstab of ",cinp$ct_var2," versus ",cinp$ct_var1, sep = ""), 
	# 			x = '', y = "Count", fill = cinp$ct_var2))

	do.call(grid.arrange, c(plots, list(ncol = 1)))
}
