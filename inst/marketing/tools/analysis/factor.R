###############################
# Pre-factor analysis
###############################
output$uiPreFactor_vars <- renderUI({
	# variable selection
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  # if(length(vars) == 0) return()
  selectInput(inputId = "preFactor_vars", label = "Variables:", choices = vars,
  	selected = state_multiple("preFactor_vars",vars),
  	multiple = TRUE, size = min(15, length(vars)), selectize = FALSE)
})

output$ui_preFactor <- renderUI({
	# for ui
  list(
  	wellPanel(
	  	uiOutput("uiPreFactor_vars")
	  ),
 		helpAndReport('Pre-factor','preFactor',inclMD("tools/help/preFactor.md"))
 	)
})

output$preFactor <- renderUI({
	# for input-output
  statTabPanel("Factor","Pre-factor",".preFactor", "preFactor")
})

.preFactor <- reactive({
	if(input$preFactor_vars %>% not_available)
		return("This analysis requires multiple variables of type numeric or integer.\nIf these variables are not available please select another dataset.")
	if(length(input$preFactor_vars) < 2) return("Please select two or more numeric variables")
	preFactor(input$dataset, input$preFactor_vars)
})

observe({
  if(input$preFactorReport %>% not_pressed) return()
  isolate({
		inp <- list(input$dataset, input$preFactor_vars)
		updateReport(inp,"preFactor")
  })
})

preFactor <- function(dataset, preFactor_vars) {
	# for main analysis

	# load("~/gh/radiant_dev/inst/marketing/data/data_examples/anscombe.rda")
	# r_data <- list()
	# dataset <- "anscombe"
	# r_data[[dataset]] <- anscombe
	# preFactor_vars <- colnames(anscombe)[1:6]
	# library(psych)

	dat <- select_(r_data[[dataset]], .dots = preFactor_vars)
	if(nrow(dat) <= ncol(dat)) return("Data should have more observations than variables.\nPlease reduce the number of variables.")

  cmat <- cor(dat)
	btest <- cortest.bartlett(cmat, nrow(dat))
	pre_kmo <- KMO(cmat)
	pre_eigen <- eigen(cmat)$values

  err_mess <- "The selected variables are perfectly collinear. Please check the correlations\nand remove any variable with a correlation of 1 or -1 from the analysis"
  if(det(cmat) > 0) {
    # pre_r2 <- data.frame(1 - (1 / diag(solve(cmat))))
    scmat <- try(solve(cmat), silent = TRUE)
    if(is(scmat, 'try-error')) {
    	pre_r2 <- err_mess
    } else {
    	pre_r2 <- {1 - (1 / diag(scmat))} %>%
    							data.frame %>%
    							set_colnames('Rsq')
    }
  } else {
  	pre_r2 <- err_mess
  }

	list(btest = btest, pre_eigen = pre_eigen, pre_kmo = pre_kmo, pre_r2 = pre_r2)
}

# Generate output for the summary tab
summary_preFactor <- function(result = .preFactor()) {

	if(result$pre_r2 %>% is.character) {
		cat(result$pre_r2)
		return(invisible())
	}

	# for summary
	btest <- result$btest
	cat("Pre-factor analysis diagnostics:\n\n")
	cat("Bartlett test of sphericity\n")
	cat("Chi-square: ", round(btest$chisq,2), "\n")
	cat("Degrees of freedom: ", btest$df, "\n")
  bt <- round(btest$p.value,3)
  cat("p-value: ", ifelse(bt < .001, "< .001", bt), "\n")
	cat("H0: Correlation Matrix = Identity Matrix, i.e., variables are not correlated\n")

	cat("\nKaiser-Meyer-Olkin measure of sampling adequacy\nKMO: ", round(result$pre_kmo$MSA,2), "\n")
	# cat("\nMeasures of sampling adequacy:\n")
	# print(result$pre_kmo$MSAi, digits = 3)

  cat("\nVariable collinearity:\n")
  print(round(result$pre_r2, 2), digits = 2)

	cat("\n")
	ev <- result$pre_eigen
	ev.var <- ev/sum(ev)
	ev.cvar <- cumsum(ev.var)
	df <- data.frame(1:length(ev),round(ev,2),round(ev.var,2),round(ev.cvar,2))
	colnames(df) <- c("Factor","Eigen Values","% of variance","Cumulative %")
  print(df, row.names = FALSE, digits = 2)
}

# Generate output for the plots tab
plots_preFactor <- function(result = .preFactor()){

	if(result$pre_r2 %>% is.character) return()

	ev <- result$pre_eigen

	p <- ggplot(data.frame(ev), aes(x=1:length(ev), y=ev, group = 1)) +
			  geom_line(colour="blue", linetype = 'dotdash', size=.7) +
  		  geom_point(colour="blue", size=4, shape=21, fill="white") +
	  		geom_hline(yintercept = 1, color = 'black', linetype = 'solid', size = 1) +
	  	  labs(list(title = "Screeplot of Eigenvalues", x = "# of factors", y = "Eigenvalues"))
	print(p)
}

###############################
# Factor analysis
###############################
output$uiFactor_vars <- renderUI({

 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  selectInput(inputId = "factor_vars", label = "Variables:", choices = vars,
  	selected = state_multiple("factor_vars", vars, input$preFactor_vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

fac_method <- c('Principal components' = 'PCA', 'Maximum Likelihood' = "maxlik")
fac_rotation <- c('Varimax' = 'varimax', 'None' = 'none')

output$ui_fullFactor <- renderUI({
	# for ui
	list(
    wellPanel(
      uiOutput("uiFactor_vars"),
      selectInput("fac_method", label = "Method:", choices = fac_method,
      	selected = state_single("fac_method", fac_method, "PCA")),
			div(class="row",
 	    	div(class="col-xs-6", numericInput("fac_number",
 	    	    label = "# of factors:", min = 1,
 	    	    value = state_init('fac_number',1))),
	    	div(class="col-xs-6", numericInput("fac_cutoff",
	    	    label = "Cutt-off", min = 0, max = 1,
	    	    value = state_init('fac_cutoff',0), step = .05))
  	  ),
  	  conditionalPanel(condition = "input.tabs_fullFactor == 'Summary'",
        checkboxInput("fac_sort", "Sort", value = state_init('fac_sort',FALSE))
      ),
      radioButtons("fac_rotation", label = "Rotation:", fac_rotation,
      	selected = state_single("fac_rotation", fac_rotation, "varimax"),
      	inline = TRUE),
      actionButton("fac_savescores", "Save scores")
  	),
		helpAndReport('Factor','fullFactor',inclMD("tools/help/fullFactor.md"))
	)
})

fac_plotWidth <- function() {
	result <- .fullFactor()
	ifelse(is.list(result), return(result$plotWidth), return(650))
}

fac_plotHeight <- function() {
	result <- .fullFactor()
	ifelse(is.list(result), return(result$plotHeight), return(650))
}

output$fullFactor <- renderUI({
	# for input-output
  statTabPanel("Factor","Factor",".fullFactor", "fullFactor", "fac_plotWidth", "fac_plotHeight")
})

.fullFactor <- reactive({

	if(input$factor_vars %>% not_available)
		return("This analysis requires multiple variables of type\nnumeric or integer.\nIf these variables are not available please select another dataset.")

	if(length(input$factor_vars) < 2) return("Please select two or more variables")
	if(is.null(input$fac_number)) return("Number of factors should be > 1.")

	fullFactor(input$dataset, input$factor_vars, input$fac_method, input$fac_number, input$fac_cutoff,
		input$fac_sort, input$fac_rotation)
})

observe({
  if(is.null(input$fullFactorReport) || input$fullFactorReport == 0) return()
  isolate({

		inp <- list(input$dataset, input$factor_vars, input$fac_method, input$fac_number, input$fac_cutoff,
			input$fac_sort, input$fac_rotation)

		# extra command to save factor scores
		xcmd <- paste0("saveFactorScores(result)")

		# updateReport(inp,"fullFactor", xcmd = xcmd)
		updateReport(inp,"fullFactor", round(7 * fac_plotWidth()/650,2), round(7 * fac_plotHeight()/650,2), xcmd = xcmd)
  })
})

fullFactor <- function(dataset, factor_vars, fac_method, fac_number, fac_cutoff,
			fac_sort, fac_rotation) {

	dat <- na.omit( r_data[[dataset]][,factor_vars] )
	if(nrow(dat) <= ncol(dat)) return("Data should have more observations than variables.\nPlease reduce the number of variables.")

	nrFac <- max(1,as.numeric(fac_number), na.rm = TRUE)
	if(nrFac > ncol(dat)) {
		cat("The number of factors cannot exceed the number of variables.\n")
		nrFac <- ncol(dat)
	}

	if(fac_method == 'PCA') {
		fres <- principal(dat, nfactors=nrFac, rotate=fac_rotation, scores=TRUE, oblique.scores=FALSE)
	} else {
		fres <- factanal(dat, nrFac, rotation=fac_rotation, scores='regression')
		fres$rotation <- fac_rotation
	}

	fres$fac_sort <- fac_sort
	fres$fac_cutoff <- fac_cutoff

	nr.plots <- (nrFac * (nrFac - 1)) / 2
	ifelse(nr.plots > 2, fres$plotHeight <- 350 * ceiling(nr.plots/2), fres$plotHeight <- 350)
	ifelse(nr.plots > 1, fres$plotWidth <- 700, fres$plotWidth <- 350)

	fres
}

# Generate output for the summary tab
summary_fullFactor <- function(result = .fullFactor()) {

	cat("Factor loadings matrix:\n")
	ifelse(result$fac_sort, loadings <- fa.sort(result$loadings), loadings <- result$loadings)
	print(loadings, cutoff = result$fac_cutoff, digits = 2)

	communalities <- data.frame(1 - result$uniqueness)
	colnames(communalities) <- ""
	cat("\nAttribute communalities:\n")
	print(round(communalities, 2), digits = 2)

	cat("\nFactor scores (max 30 shown):\n")
	scores <- as.data.frame(result$scores)
  print(round(scores[1:min(nrow(scores),30),, drop = FALSE], 2))
}

# Generate output for the plots tab
plots_fullFactor <- function(result = .fullFactor()) {

	# when no analysis was conducted (e.g., no variables selected)
	if(result$factors < 2) result <- "Plots require two or more factors"
	if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))

	df <- round(as.data.frame(result$loadings[]),3)
	rnames <- rownames(df)
	cnames <- colnames(df)
	plots <- list()
	pnr <- 1
	ab_df <- data.frame(a=c(0,0), b=c(1, 0))

	for(i in 1:(length(cnames)-1)) {
		for(j in (i+1):length(cnames)) {

			i_name <- cnames[i]
			j_name <- cnames[j]

		  df2 <- cbind(df[, c(i_name,j_name)],rnames)
  		plots[[pnr]] <- ggplot(df2, aes_string(x = i_name, y = j_name, color = 'rnames', label = 'rnames')) + geom_text() + theme(legend.position = "none") +
  			xlim(-1,1) + ylim(-1,1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
  		pnr <- pnr + 1
  	}
	}
	do.call(grid.arrange, c(plots, list(ncol = min(2,length(plots)))))
}

# save factor scores when action button is pressed
observe({
	if(input$fac_savescores %>% not_pressed) return()
	isolate({
		result <- .fullFactor()
		if(is.character(result)) return()
		data.frame(result$scores) %>% changedata(paste0("fac",1:ncol(.)))
	})
})
