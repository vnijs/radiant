###############################
# Correlation
###############################
cor_type <- c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")

output$ui_correlation <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiCor_var"),
		  selectInput(inputId = "cor_var", label = "Select variables:", choices = varnames(), 
  	  	selected = state_multvar("cor_var",varnames()), multiple = TRUE, selectize = FALSE),
		  selectInput(inputId = "cor_type", label = "Method", choices = cor_type, 
  	  	selected = state_init_list("cor_type","pearson", cor_type), multiple = FALSE),
     	numericInput("cor_cutoff", label = "Correlation cutoff:", min = 0, max = 1, 
    		value = state_init('cor_cutoff',0), step = 0.05)
	  ),
	 	helpAndReport('Correlation','correlation',inclMD("tools/help/correlation.md"))
	)
})

cor_plotWidth <- function() {
	result <- .correlation()
	ifelse(is.list(result), return(result$plotWidth), return(650))
}

cor_plotHeight <- function() {
	result <- .correlation()
	ifelse(is.list(result), return(result$plotHeight), return(650))
}

output$correlation <- renderUI({
	# create inputs and outputs - function in radiant.R
  statTabPanel("Regression","Correlation",".correlation","correlation", "cor_plotWidth", "cor_plotHeight")
})

.correlation <- reactive({
	vars <- input$cor_var
	ret_text <- "Please select two or more variables"
	if(is.null(vars) || length(vars) < 2) return(ret_text)
	# if(is.null(inChecker(c(input$cor_var)))) return(ret_text)
	correlation(input$datasets, input$cor_var, input$cor_type, input$cor_cutoff)
})

observe({
  if(is.null(input$correlationReport) || input$correlationReport == 0) return()
  isolate({
		inp <- list(input$datasets, input$cor_var, input$cor_type, input$cor_cutoff)
		updateReport(inp,"correlation", round(7 * cor_plotWidth()/650,2), round(7 * cor_plotHeight()/650,2))
  })
})

correlation <- function(datasets, cor_var, cor_type, cor_cutoff) {

	dat <- na.omit( values[[datasets]][,cor_var] )
	dat <- data.frame(lapply(dat,as.numeric))

	nc <- ncol(dat)
	list('dat' = dat, 'cor_type' = cor_type, 'cor_cutoff' = cor_cutoff, 
		'plotHeight' = 150 * nc,  'plotWidth' = 150 * nc)
}

summary_correlation <- function(result = .correlation()) {
	
	dat <- result$dat

	# calculate the correlation matrix with p-values using the psych package
	cmat <- corr.test(dat, method = result$cor_type)

	cr <- format(round(cmat$r,2))
  cr[abs(cmat$r) < result$cor_cutoff] <- ""
	ltmat <- lower.tri(cr)
  cr[!ltmat] <- ""

	cp <- format(round(cmat$p,2))
  cp[abs(cmat$r) < result$cor_cutoff] <- ""
  cp[!ltmat] <- ""

	cat("Correlation matrix:\n")
  print(cr, quote = FALSE)
	cat("\np-values:\n")
  print(cp, quote = FALSE)
}

plots_correlation <- function(result = .correlation()) {

	if(class(result) == 'list') dat <- result$dat

	# based mostly on http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=137
	panel.plot <- function(x, y) {
	    usr <- par("usr"); on.exit(par(usr))
	    par(usr = c(0, 1, 0, 1))
	    ct <- cor.test(x,y, method = result$cor_type)
	    sig <- symnum(ct$p.value, corr = FALSE, na = FALSE,
	                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
	                  symbols = c("***", "**", "*", ".", " "))
	    r <- ct$estimate
	    rt <- format(r, digits=2)[1]
	    cex <- 0.5/strwidth(rt)
	    
	    text(.5, .5, rt, cex=cex * abs(r))
	    text(.8, .8, sig, cex=cex, col='blue')
	}
	panel.smooth <- function (x, y) {
    points(x, y)
    abline(lm(y~x), col="red")
    lines(stats::lowess(y~x), col="blue")
	}
	pairs(dat, lower.panel=panel.smooth, upper.panel=panel.plot)
}

################################################################
# OLS
################################################################
output$uiReg_var1 <- renderUI({
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "reg_var1", label = "Dependent variable:", choices = vars, 
  	selected = state_singlevar("reg_var1",vars), multiple = FALSE)
})

output$uiReg_var2 <- renderUI({
  if(is.null(input$reg_var1)) return()
	notChar <- "character" != getdata_class()
  vars <- varnames()[notChar]
 	vars <- vars[-which(vars == input$reg_var1)]
  if(length(vars) == 0) return()
  selectInput(inputId = "reg_var2", label = "Independent variables:", choices = vars, 
  	selected = state_multvar("reg_var2", vars), multiple = TRUE, selectize = FALSE)
})

output$uiReg_var3 <- renderUI({
  # if(is.null(input$reg_var2)) return()
  vars <- varnames()
 	vars <- vars[which(vars %in% input$reg_var2)]

  # adding interaction terms as needed 
	if(!is.null(input$reg_intsel) && input$reg_interactions != 'none') vars <- c(vars,input$reg_intsel)

  selectInput(inputId = "reg_var3", label = "Variables to test:", choices = vars, 
  	selected = state_multvar("reg_var3", vars), multiple = TRUE, selectize = FALSE)
})

output$uiReg_intsel <- renderUI({

  vars <- input$reg_var2
  if(is.null(vars) || length(vars) < 2) return()

  # choices <- ""
 	# if(!is.null(inChecker(c(input$reg_var2)))) choices <- reg_int_vec(vars,input$reg_interactions)
 	choices <- reg_int_vec(vars,input$reg_interactions)

	selectInput("reg_intsel", label = "", choices = choices, 
  	selected = state_multvar("reg_intsel", vars), multiple = TRUE, selectize = FALSE)
})

reg_interactions <- c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way")

output$ui_regression <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiReg_var1"),
	    uiOutput("uiReg_var2"),

	    # conditionalPanel(condition = "input.reg_var2 != null",
		  	checkboxInput(inputId = "reg_standardize", label = "Standardized coefficients", 
	    		value = state_init('reg_standardize',FALSE)),
		    radioButtons(inputId = "reg_interactions", label = "Interactions:", reg_interactions, 
	  	  	selected = state_init_list("reg_interactions","none", reg_interactions)),
		    conditionalPanel(condition = "input.reg_interactions != 'none'",
		  		uiOutput("uiReg_intsel")
		  	),
		    conditionalPanel(condition = "input.tabs_regression == 'Summary'",
			    uiOutput("uiReg_var3"),
			    # checkboxInput(inputId = "reg_outlier", label = "Outlier test", value = FALSE),
			    checkboxInput(inputId = "reg_vif", label = "Calculate VIF-values", 
	  	  		value = state_init('reg_vif',FALSE)),
		  	  checkboxInput(inputId = "reg_stepwise", label = "Select variables step-wise", 
		    		value = state_init('reg_stepwise',FALSE))
		  	),
		    conditionalPanel(condition = "input.tabs_regression == 'Plots'",
		      selectInput("reg_plots", "Regression plots:", choices = r_plots, 
		  	  	selected = state_init_list("reg_plots","", r_plots))
		    ),
		    actionButton("saveres", "Save residuals")
	    # )
	  ),
		helpAndReport('Regression','regression', inclMD("tools/help/regression.md"))
	)
})

reg_plotWidth <- function() {
	result <- .regression()
	ifelse(class(result) == 'lm', return(result$plotWidth), return(650))
}

reg_plotHeight <- function() {
	result <- .regression()
	ifelse(class(result) == 'lm', return(result$plotHeight), return(650))
}

output$regression <- renderUI({
	# create inputs and outputs - function in radiant.R
  statTabPanel("Regression","Linear (OLS)",".regression","regression", "reg_plotWidth", "reg_plotHeight")
})

.regression <- reactive({
	if(is.null(input$reg_standardize)) return("")
	ret_text <- "This analysis requires a dependent variable of type integer\nor numeric and one or more independent variables.\nPlease select another dataset."
	if(is.null(input$reg_var1)) return(ret_text)
	if(is.null(input$reg_var2)) return("Please select one or more independent variables.")

	result <- regression(input$datasets, input$reg_var1, input$reg_var2, input$reg_var3, input$reg_intsel, 
		input$reg_interactions, input$reg_standardize, input$reg_vif, input$reg_stepwise, input$reg_plots)

	# specifying plot heights
	nrVars <- length(as.character(attr(result$terms,'variables'))[-1])

	result$plotHeight <- 650
	result$plotWidth <- 650

	if(input$reg_plots == 'histlist') result$plotHeight <- 325 * ceiling(nrVars / 2)

	if(input$reg_plots == 'correlations') {
		result$plotHeight <- 150 * nrVars
		result$plotWidth <- 150 * nrVars
	}

	if(input$reg_plots %in% c('scatterlist','leverage_plots','resid_vs_predictorlist')) {
		result$plotHeight <- 325 * ceiling((nrVars-1) / 2)
	}

	result	
})

observe({
  if(is.null(input$regressionReport) || input$regressionReport == 0) return()
  isolate({
		inp <- list(input$datasets, input$reg_var1, input$reg_var2, input$reg_var3, input$reg_intsel,
			input$reg_interactions, input$reg_standardize, input$reg_vif, input$reg_stepwise, input$reg_plots)
		updateReport(inp,"regression", round(7 * reg_plotWidth()/650,2), round(7 * reg_plotHeight()/650,2))
  })
})

regression <- function(datasets, reg_var1, reg_var2, reg_var3, reg_intsel, reg_interactions, reg_standardize, 
	reg_vif, reg_stepwise, reg_plots) {

	vars <- reg_var2

	# adding interaction terms as needed
	if(reg_interactions != 'none' && !is.null(reg_intsel) && length(vars) > 1) {
		vars <- c(vars,reg_intsel)
	}

	dat <- values[[datasets]]
	if(reg_standardize) dat <- data.frame(lapply(dat,reg_standardize))

	formula <- paste(reg_var1, "~", paste(vars, collapse = " + "))

	if(reg_stepwise) {
		mod <- step(lm(as.formula(paste(reg_var1, "~ 1")), data = dat), scope = list(upper = formula), direction = 'forward')
		# mod <- step(lm(as.formula(paste(reg_var1, "~ 1")), data = dat), k = log(nrow(dat)), scope = list(upper = formula), direction = 'both')
	} else {
		mod <- lm(formula, data = dat)
	}

	mod$reg_vif <- reg_vif
	mod$reg_var2 <- reg_var2
	mod$reg_var3 <- reg_var3
	mod$reg_stepwise <- reg_stepwise
	mod$reg_plots <- reg_plots
	mod$reg_interactions <- reg_interactions
	mod$reg_intsel <- reg_intsel
	mod$reg_standardize <- reg_standardize
	mod$datasets <- datasets

	return(mod)
}

# summary_regression <- reactive({ .summary_regression() })
summary_regression <- function(result = .regression()) {

	if(class(result) != 'lm') return(result)

	# rounding to avoid scientific notation for the coefficients
	res <- summary(result)
	res$coefficients <- round(res$coefficients,3)
	print(res)

	# if(reg_outlier) print(outlierTest(result), digits = 3)

	if(result$reg_vif) {
		print(vif_regression(result), digits = 3)
	} 

	if(!is.null(result$reg_var3)) {
		if(!result$reg_stepwise) {
			test_regression(result)
		} else {
	  	cat("Model comparisons not conducted when Stepwise has been selected.\n")
	  }
	}
}

# main functions called from radiant.R
r_plots <- list("None" = "", "Histograms" = "histlist", "Correlations" = "correlations", "Scatter" = "scatterlist", "Dashboard" = "dashboard",
		# "Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots", "Coefficient plot" = "coef")
		"Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots")

# plots_regression <- reactive({ .plots_regression() })
plots_regression <- function(result = .regression()) {

	if(class(result) != 'lm') return(result)
	mod <- ggplot2::fortify(result)

	vars <- as.character(attr(result$terms,'variables'))[-1]
	reg_var1 <- vars[1]
	reg_var2 <- vars[-1]

	dat <- mod[,vars, drop = FALSE]

	if(result$reg_plots == "") 
		return(plot(x = 1, type = 'n', main="Please select a plot from the Regression plots dropdown menu.", axes = FALSE, xlab = "", ylab = ""))

	if(result$reg_plots == "histlist") {
		plots <- list()
		for(i in vars) plots[[i]] <- ggplot(dat, aes_string(x = i)) + geom_histogram()
	}

	if(result$reg_plots == "correlations") {
		datlist <- list()
		datlist$dat <- dat
		return(plots_correlation(datlist))
	}

	if(result$reg_plots == "dashboard") {

		plots <- list()
		df <- data.frame(cbind(mod$.fitted,mod[1]))
		colnames(df) <- c("x","y")
		plots[[1]] <- ggplot(df, aes(x=x, y=y)) + geom_point() + geom_abline(linetype = 'dotdash') +
			geom_smooth(size = .75, linetype = "dotdash") + labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual values"))

		plots[[2]] <- qplot(.fitted, .resid, data = mod) + geom_smooth(size = .75, linetype = "dotdash") +
			labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))

		plots[[3]] <- qplot(y=.resid, x=seq_along(.resid), data = mod) + geom_point() + 
			geom_smooth(size = .75, linetype = "dotdash") + labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))

		plots[[4]] <- qplot(sample =.stdresid, data = mod, stat = "qq") + geom_abline(linetype = 'dotdash') +
			labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))
	}

	if(result$reg_plots == "scatterlist") {
		plots <- list()
		for(i in reg_var2) { 
			# if(getdata_class()[i] == 'factor') {
			if('factor' %in% class(dat[,i])) {
				plots[[i]] <- ggplot(dat, aes_string(x=i, y=reg_var1, fill=i)) + geom_boxplot(alpha = .3)
			} else {
				plots[[i]] <- ggplot(dat, aes_string(x=i, y=reg_var1)) + geom_point() + geom_smooth(size = .75, linetype = "dotdash")
			}
		}
	}

	if(result$reg_plots == "resid_vs_predictorlist") {
		plots <- list()
		residuals <- mod$.resid
		rdat <- cbind(residuals,dat[,reg_var2])
		rdat <- data.frame(rdat)
		colnames(rdat) <- c('residuals',reg_var2)
		for(i in reg_var2) {
			# if(getdata_class()[i] == 'factor') {
			if('factor' %in% class(dat[,i])) {
				plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_boxplot(fill = 'blue', alpha = .3)
			} else {
				plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_point() + geom_smooth(size = .75, linetype = "dotdash")
			}
		}
	}

	if(result$reg_plots == "leverage_plots") {
		return(leveragePlots(result, main = "", ask=FALSE, id.n = 1, layout = c(ceiling(length(reg_var2)/2),2)))
	}

	# coefficient plots require the arm package (Gelman)
	# if(result$reg_plots == "coef") {
	# 	return(coefplot(result, xlab="", ylab="", main="Coefficient plot", col.pts="blue", CI=2))
	# } 

	if(exists("plots"))
		suppressWarnings(suppressMessages(do.call(grid.arrange, c(plots, list(ncol = 2)))))
}

saveRegResiduals <- function(result = .regression()) {
	resid <- data.frame(result$residuals)
	changedata(resid, 'residuals')
}

# save residuals
observe({
	if(is.null(input$saveres) || input$saveres == 0) return()
	isolate({
		result <- .regression()
		if(is.character(result)) return()
		saveRegResiduals(result)
	})
})

################################################################
# Additional functions for regression
################################################################
reg_standardize <- function(x) {
	if(is.factor(x)) return(x)
	if(is.numeric(x)) return(scale(x))
}

reg_int_vec <- function(reg_vars, nway) {
	n <- length(reg_vars)
	iway <- c()
	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			iway <- c(iway, paste(reg_vars[i],reg_vars[j],sep=":"))
		}
	}
	if(n >= 3 && nway == '3way') {
		for(i in 1:(n-2)) {
			for(j in (i+1):(n-1)) {
				for(k in (j+1):n) {
					iway <- c(iway, paste(reg_vars[i],reg_vars[j],reg_vars[k],sep=":"))
				}
			}
		}
	}
	iway
}

vif_regression <- function(result = .regression()) {
	if(result$reg_vif) {
		if(length(result$reg_var2) > 1) {
	  	cat("Variance Inflation Factors\n")

	  	VIF <- vif(result)
	  	if(!is.null(dim(VIF))) VIF <- VIF[,'GVIF'] # needed when factors are included
	  	VIF <- sort(VIF, decreasing = TRUE)

	  	ifelse(length(VIF) < 8, return(VIF), return(data.frame(VIF)))
		} else {
	  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
		}
	}
}

test_regression <- function(result = .regression()) {
	dat <- values[[result$datasets]]
	if(result$reg_standardize) dat <- data.frame(lapply(dat,reg_standardize))

	sub_formula <- ". ~ 1"
	vars <- result$reg_var2

  if(result$reg_interactions != 'none' && !is.null(result$reg_intsel) && length(vars) > 1) {
		vars <- c(vars,result$reg_intsel)
	}

	not_selected <- setdiff(vars,result$reg_var3)
	if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))

	reg_sub <- update(result, sub_formula)
	anova(reg_sub, result, test='F')
}

