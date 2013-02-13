# Pre-factor {{{

.preFactorAnalysisCheckFunction <- function(state){

	if(length(state$vars) < 2)
		return("Please select at least two variables")
	cmd <- paste("dim.data <- dim(", state$data, "[,", paste("c(\"",paste(state$vars,sep = "", collapse = "\",\""),"\")", sep = ""),"])")
	eval(parse(text = cmd))
	if(dim.data[1] < dim.data[2])
		return("Data has more variables than observations. Please reduce the number of variables.")
	return("")
}

.preFactorAnalysisRunFunction <- function(state) {
	
	cmd <- paste("pf.data <-", state$data, "[,", paste("c(\"",paste(state$vars,sep = "", collapse = "\",\""),"\")", sep = ""),"]")
	eval(parse(text = cmd))

	btest <- cortest.bartlett(cor(pf.data), dim(pf.data)[1])
	prefac <- paf(as.matrix(pf.data))

	output <- paste("\n\nPre-factor analysis diagnostics:\n\n")
	output <- paste(output, "Bartlett test of sphericity\n", sep = "")
	output <- paste(output, "Chi-square: ", round(btest$chisq,3), "\n", sep = "")
	output <- paste(output, "Degrees of freedom: ", btest$df, "\n",sep = "")
	output <- paste(output, "p-value: ", print(round(btest$p.value,10), digits = 3), "\n",sep = "")
	output <- paste(output, "H0: Correlation Matrix = Identity Matrix\n", sep = "")
	output <- paste(output, "i.e., variables are not correlated with one another\n", sep = "")

	output <- paste(output, "\nKaiser-Meyer-Olkin measure of sampling adequacy\nKMO: ", round(prefac$KMO,3), "\n", sep = "")
	output <- paste(output, "\nMeasures of sampling adequacy:\n", sep = "")
	eval(parse(text = "cat(output)"))
	eval(parse(text = "print(prefac$MSA, digits = 3)"))

	ev <- prefac$Eigenvalues[,'0']
	ev.var <- ev/sum(ev)
	ev.cvar <- cumsum(ev.var)
	df <- data.frame(1:length(ev),ev,ev.var,ev.cvar)
	colnames(df) <- c("Factors","Eigen Values","% of variance","Cumulative %") 
	eval(parse(text = "print(df, digits = 3)"))

	JavaGD(name="Screeplot", width=600, height=600, ps=12)
	plot(ev, type = 'b', col = 'blue', main = "Screeplot of Eigenvalues", ylab = "Eigenvalues", xlab = "# of factors")
	abline(1,0, col = 'red')
	rm(pf.data)
} #}}}

# variable selection - hclustering
output$prefac_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "prefac_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

fac_method <- list('Principal components' = 'PCA')

ui_preFactor <- function() {
  wellPanel(
    uiOutput("prefac_vars"), 
    selectInput("fac_method", label = "Method:", choices = fac_method, selected = fac_method[1], multiple = FALSE),
  )
}

summary.hclustering <- function(result) {
	result
}

plot.hclustering <- function(result) {

}

hclustering <- reactive(function() {
	if(is.null(input$hc_vars)) return("Please select one or more variables")

	# dist.data <- as.dist(dist(getdata()[,input$hc_vars], method = "euclidean")^2)
	dat <- getdata()[,input$hc_vars]
	if(input$hc_dist == "sq.euclidian") {
		dist.data <- dist(dat, method = "euclidean")^2
	} else {
		dist.data <- dist(dat, method = input$hc_dist)
	}
	hclust(d = dist.data, method= input$hc_meth)
})

observe(function() {
	if(is.null(input$hc_saveclus) || input$hc_saveclus == 0) return()
	isolate({
		clusmem <- cutree(hclustering(), k = input$hc_nrClus)
		changedata(as.factor(clusmem), paste("hclus",input$hc_nrClus,sep=""))
	})
})
