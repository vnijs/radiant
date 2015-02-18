# alternative hypothesis options
base_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

###############################
# Cross-tabs
###############################
output$uiCt_var1 <- renderUI({
	isFct <- "factor" == getdata_class()
  vars <- varnames()[isFct]
  selectInput(inputId = "ct_var1", label = "Select a grouping factor:", choices = vars,
  	selected = state_single("ct_var1",vars), multiple = FALSE)
})

output$uiCt_var2 <- renderUI({
	isFct <- "factor" == getdata_class()
  vars <- varnames()[isFct]
  if(length(vars) > 0) vars <- vars[-which(vars == input$ct_var1)]
  selectInput(inputId = "ct_var2", label = "Select a factor:", choices = vars,
  	selected = state_single("ct_var2",vars), multiple = FALSE)
})

output$ui_crosstab <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiCt_var1"),
	    uiOutput("uiCt_var2"),
		  checkboxInput("ct_observed", label = "Observed values",
	     	value = state_init("ct_observed",TRUE)),
		  checkboxInput("ct_expected", label = "Expected values",
	     	value = state_init("ct_expected",FALSE)),
	    conditionalPanel(condition = "input.tabs_crosstab == 'Summary'",
			  checkboxInput("ct_contrib", label = "Contribution to chisquare value",
	     	value = state_init("ct_contrib",FALSE))),
		  checkboxInput("ct_std_residuals", label = "Deviation (standarized)",
	     	value = state_init("ct_std_residuals",FALSE)),
		  checkboxInput("ct_deviation", label = "Deviation (percentage)",
	     	value = state_init("ct_deviation",FALSE))
		),
	 	helpAndReport("Cross-tabs","crosstab",inclMD("../quant/tools/help/crossTabs.md"))
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
	if(input$ct_var2 %>% not_available)
		return("This analysis requires variables of type factor.\nIf none are available please select another dataset.")

	do.call(crosstab, ct_inputs())
})

observe({
  if(input$crosstabReport %>% not_pressed) return()
  isolate({
		updateReport(ct_inputs() %>% clean_args, "crosstab",
		             round(7 * ct_plotWidth()/650,2), round(7 * ct_plotHeight()/650,2))
  })
})

crosstab <- function(dataset, ct_var1, ct_var2,
                     data_filter = "",
                     show_filter = FALSE,
                     ct_observed = TRUE,
                     ct_expected = FALSE,
                     ct_deviation = FALSE,
                     ct_std_residuals = FALSE,
                     ct_contrib = FALSE) {

	dat <- getdata_exp(dataset, c(ct_var1, ct_var2), filt = data_filter)

	dnn = c(paste("Group(",ct_var1,")",sep = ""), paste("Variable(",ct_var2,")",sep = ""))
	tab <- table(dat[,ct_var1], dat[,ct_var2], dnn = dnn)
	cst <- suppressWarnings( chisq.test(tab, correct = FALSE) )

	# dat no longer needed
	rm(dat)

	# adding the % deviation table
	o <- cst$observed
	e <- cst$expected
	cst$deviation <- (o-e) / e

	nrPlot <- sum(c(ct_observed, ct_expected, ct_deviation, ct_std_residuals))
	plotWidth = 650
	plotHeight = 400 * nrPlot

  environment() %>% as.list %>% set_class(c("crosstab",class(.)))

}

# test
# source("~/gh/radiant_dev/R/radiant.R")
# result <- crosstab("diamonds","cut","clarity")

# list of function arguments
ct_args <- as.list(formals(crosstab))

# list of function inputs selected by user
ct_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(ct_args))
    ct_args[[i]] <- input[[i]]
  if(!input$show_filter) ct_args$data_filter = ""
  ct_args
})

summary_crosstab <- function(result = .crosstab()) {

  cat("Cross-tabs\n")
	cat("Data     :", result$dataset, "\n")
	if(result$show_filter) {
		if(result$data_filter %>% gsub("\\s","",.) != "")
			cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
	}
	cat("Variables:", paste0(c(result$ct_var1, result$ct_var2), collapse=", "), "\n")
	cat("Null hyp.: there is no association between", result$ct_var1, "and", result$ct_var2, "\n")
	cat("Alt. hyp.: there is an association between", result$ct_var1, "and", result$ct_var2, "\n\n")

	result$cst$observed %>% rownames %>% c(., "Total") -> rnames
	result$cst$observed %>% colnames %>% c(., "Total") -> cnames

	if(result$ct_observed) {
		cat("Observed values:\n")

		result$cst$observed %>%
			rbind(colSums(.)) %>%
			set_rownames(rnames) %>%
			cbind(rowSums(.)) %>%
			set_colnames(cnames) %>%
			print
	}

	if(result$ct_expected) {
		cat("\nExpected values:\n")

		result$cst$expected %>%
			rbind(colSums(.)) %>%
			set_rownames(rnames) %>%
			cbind(rowSums(.)) %>%
			set_colnames(cnames) %>%
			round(2) %>%
			print

	}
	if(result$ct_contrib) {
		cat("\nContribution to chisquare value:\n")
		print((result$cst$observed - result$cst$expected)^2 / result$cst$expected, digits = 2)
	}
	if(result$ct_std_residuals) {
		cat("\nDeviation (standardized):\n")
		print(round(result$cst$residuals, 2)) 	# these seem to be the correct std.residuals
	}
	if(result$ct_deviation) {
		cat("\nDeviation (percentage):\n")
		print(round(result$cst$deviation, 2)) 	# % deviation
	}
	# if(result$ct_cellperc) {
	# 	cat("\nCell percentages:\n")
	# 	print(prop.table(result$table), digits = 2)  	# cell percentages
	# }
	# if(result$ct_rowperc) {
	# 	cat("\nRow percentages:\n")
	# 	print(prop.table(result$table, 1), digits = 2) # row percentages
	# }
	# if(result$ct_colperc) {
	# 	cat("\nColumn percentages:\n")
	# 	print(prop.table(result$table, 2), digits = 2) # column percentages
	# }

	print(result$cst)
	cat(paste(sprintf("%.1f",100 * (sum(result$cst$expected < 5) / length(result$cst$expected))),"% of cells have expected values below 5\n\n"), sep = "")
}

plots_crosstab <- function(result = .crosstab()) {

	gather_table <- function(tab) {
		tab %>%
			data.frame %>%
			mutate(rnames = rownames(.)) %>%
			gather_("variable", "values")
	}

	plots <- list()
	if(result$ct_std_residuals) {

		tab <- gather_table(result$cst$residuals)
		colnames(tab)[1:2] <- c(result$ct_var1, result$ct_var2)
		plots[['residuals']] <- ggplot(tab, aes_string(x = result$ct_var1, y = "values", fill = result$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) +
     					geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
     					geom_text(data = NULL, x = 1, y = 2.11, label = "95%") +
     					geom_text(data = NULL, x = 1, y = 1.49, label = "90%") +
         			labs(list(title = paste("Deviation (standardized) for ",result$ct_var2," versus ",result$ct_var1, sep = ""), x = result$ct_var1))
	}

	if(result$ct_deviation) {

		tab <- gather_table(result$cst$deviation)
		colnames(tab)[1:2] <- c(result$ct_var1, result$ct_var2)
		plots[['deviation']] <- ggplot(tab, aes_string(x = result$ct_var1, y = "values", fill = result$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) + ylim(-1,1) +
         			labs(list(title = paste("Deviation (percentage) for ",result$ct_var2," versus ",result$ct_var1, sep = ""), x = result$ct_var1))
	}

	if(result$ct_expected) {

		fact_names <- result$cst$expected %>% dimnames %>% as.list
  	tab <- gather_table(result$cst$expected)
		tab$rnames %<>% as.factor %>% factor(levels = fact_names[[1]])
		tab$variable %<>% as.factor %>% factor(levels = fact_names[[2]])

		plots[['expected']] <- ggplot(tab, aes_string(x = "rnames", y = "values", fill = "variable")) +
         			geom_bar(stat="identity", position = "fill", alpha = .7) +
         			labs(list(title = paste("Expected values for ",result$ct_var2," versus ",result$ct_var1, sep = ""),
							x = "", y = "", fill = result$ct_var2))
	}

	if(result$ct_observed) {

		fact_names <- result$cst$observed %>% dimnames %>% as.list
  	tab <- gather_table(result$cst$observed)
		colnames(tab)[1:2] <- c(result$ct_var1, result$ct_var2)
		tab$result$ct_var1 %<>% as.factor %>% factor(levels = fact_names[[1]])
		tab$result$ct_var1 %<>% as.factor %>% factor(levels = fact_names[[2]])

		plots[['stacked']] <-
		ggplot(tab, aes_string(x = result$ct_var1, y = "values", fill = result$ct_var2)) +
         			geom_bar(stat="identity", position = "fill", alpha = .7) +
         			labs(list(title = paste("Expected values for ",result$ct_var2," versus ",result$ct_var1, sep = ""),
							x = "", y = "", fill = result$ct_var2))
	}

	# plots[['observed']] <- ggplot(result$dat, aes_string(x = ct_var1, fill = ct_var2)) + geom_histogram(position = "dodge", alpha=.3) +
	# 	labs(list(title = paste("Crosstab of ",ct_var2," versus ",ct_var1, sep = ""),
	# 			x = '', y = "Count", fill = ct_var2))

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}
