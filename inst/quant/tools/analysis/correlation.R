###############################
# Correlation
###############################

cor_type <- c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")
output$uiCor_var <- renderUI({
	isChar <- "character" == getdata_class()
	vars <- varnames()[!isChar]
  if(length(vars) == 0) return()
  selectInput(inputId = "cor_var", label = "Select variables:", choices = vars,
 		selected = state_multiple("cor_var",vars),
 		multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_correlation <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiCor_var"),
		  selectInput(inputId = "cor_type", label = "Method:", choices = cor_type,
  	  	selected = state_single("cor_type", cor_type, "pearson"), multiple = FALSE),
     	numericInput("cor_cutoff", label = "Correlation cutoff:", min = 0, max = 1,
    		value = state_init('cor_cutoff',0), step = 0.05)
	  ),
	 	helpAndReport('Correlation','correlation',inclMD("../quant/tools/help/correlation.md"))
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
	"Please select two or more variables.\n\n" %>%
		suggest_data("diamonds") -> rt

	if(input$cor_var %>% not_available) return(rt)
	if(length(input$cor_var) < 2) return(rt)

	do.call(correlation, cor_inputs())
})

observe({
	if(input$correlationReport %>% not_pressed) return()
  isolate({
		updateReport(cor_inputs() %>% clean_args, "correlation", round(7 * cor_plotWidth()/650,2), round(7 * cor_plotHeight()/650,2))
  })
})

correlation <- function(dataset, cor_var,
                        data_filter = "",
                        show_filter = FALSE, 		# remove when recode is complete
                        cor_type = "pearson",
                        cor_cutoff = 0) {

	# could use data.matrix as the last step in the chain but it seems to be
	# about 25% slower on system time
	getdata_exp(dataset, cor_var, filt = data_filter) %>%
		mutate_each(funs(as.numeric)) -> dat

	nc <- ncol(dat)
	plotHeight <- 150 * nc
	plotWidth <- 150 * nc

  environment() %>% as.list %>% set_class(c("correlation",class(.)))
}

# list of function arguments
cor_args <- as.list(formals(correlation))

# list of functions inputs selecored by user
cor_inputs <- reactive({
  # loop needed because reacorive values don't allow single bracket indexing
  for(i in names(cor_args))
    cor_args[[i]] <- input[[i]]
  if(!input$show_filter) cor_args$data_filter = ""
  cor_args
})

# library(ggplot2)
# library(psych)
# r_data <- list()
# dataset <- "diamonds"
# r_data$diamonds <- diamonds[1:100,1:5]
# cor_var <- colnames(diamonds)[1:5]
# result <- correlation("diamonds",cor_var)
# summary_correlation(result)
# plots_correlation(result)

# system.time(
# 	for(i in 1:100) {
# 		print(i)
# 		correlation("diamonds",colnames(diamonds))
# 	}
# )

summary_correlation <- function(result = .correlation()) {

	# calculate the correlation matrix with p-values using the psych package
	cmat <- suppressWarnings( corr.test(result$dat, method = result$cor_type) )

	cr <- format(round(cmat$r,2))
  cr[abs(cmat$r) < result$cor_cutoff] <- ""
	ltmat <- lower.tri(cr)
  cr[!ltmat] <- ""

	cp <- format(round(cmat$p,2))
  cp[abs(cmat$r) < result$cor_cutoff] <- ""
  cp[!ltmat] <- ""

  cat("Correlation\n")
	cat("Data     :", result$dataset, "\n")
	if(result$show_filter) {
		if(result$data_filter %>% gsub("\\s","",.) != "")
			cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
	}
	cat("Variables:", paste0(result$cor_var, collapse=", "), "\n")
	cat("Null hyp.: variables x and y are not correlated\n")
	cat("Alt. hyp.: variables x and y are correlated\n\n")

	cat("Correlation matrix:\n")
  print(cr, quote = FALSE)
	cat("\np-values:\n")
  print(cp, quote = FALSE)
}


plots_correlation <- function(result = .correlation()) {

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
    # uncomment below if you want linear and loess lines
    # in the scatter plot matrix
		# abline(lm(y~x), col="red")
		# lines(stats::lowess(y~x), col="blue")
	}
	# result %>% { if(result %>% class == "list") .$dat else result } %>%
	result$dat %>% pairs(lower.panel=panel.smooth, upper.panel=panel.plot)
}
