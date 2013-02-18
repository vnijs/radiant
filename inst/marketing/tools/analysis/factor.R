# variable selection - factor analysis
output$preFactor_vars <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()

	dat <- getdata()[1,]
  isNum <- sapply(dat, is.numeric)
  vars <- vars[isNum]
  selectInput(inputId = "preFactor_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

ui_preFactor <- function() {
  wellPanel(uiOutput("preFactor_vars"))
}

# variable selection - factor analysis
output$factor_vars <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()

	dat <- getdata()[1,]
  isNum <- sapply(dat, is.numeric)
  vars <- vars[isNum]
  selectInput(inputId = "factor_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

fac_method <- list('Principal components' = 'PCA', 'Maximum Likelihood' = "maxlik")
fac_rotation <- list('Varimax' = 'varimax', 'None' = 'none')

ui_factor <- function() {
  wellPanel(
    uiOutput("factor_vars"), 
    selectInput("fac_method", label = "Method:", choices = fac_method, selected = fac_method[1], multiple = FALSE),
    selectInput("fac_rotation", label = "Rotation:", choices = fac_rotation, selected = fac_rotation[1], multiple = FALSE),
    numericInput("fac_number", label = "Number of factors:", min = 1, value = 1)
  )
}

summary.preFactor <- function(result) {

	btest <- result$btest
	prefac <- result$prefac

	cat("\n\nPre-factor analysis diagnostics:\n\n")
	cat("Bartlett test of sphericity\n")
	cat("Chi-square: ", round(btest$chisq,3), "\n")
	cat("Degrees of freedom: ", btest$df, "\n")
	cat("p-value: ", round(btest$p.value,3), "\n")
	cat("H0: Correlation Matrix = Identity Matrix, i.e., variables are not correlated\n")

	cat("\nKaiser-Meyer-Olkin measure of sampling adequacy\nKMO: ", round(prefac$KMO,3), "\n")
	cat("\nMeasures of sampling adequacy:\n")
	print(prefac$MSA, digits = 3)
	cat("\n")

	ev <- prefac$Eigenvalues[,'0']
	ev.var <- ev/sum(ev)
	ev.cvar <- cumsum(ev.var)
	df <- data.frame(1:length(ev),ev,ev.var,ev.cvar)
	colnames(df) <- c("Factor","Eigen Values","% of variance","Cumulative %") 
	print(df, digits = 3, row.names = FALSE)
}

plot.preFactor <- function(result) {

	prefac <- result$prefac
	ev <- prefac$Eigenvalues[,'0']
	plot(ev, type = 'b', col = 'blue', main = "Screeplot of Eigenvalues", ylab = "Eigenvalues", xlab = "# of factors")
	abline(1,0, col = 'red')
}

preFactor <- reactive({
	if(is.null(input$preFactor_vars) || length(input$preFactor_vars) < 2) return("Please select two or more variables")

	dat <- getdata()[,input$preFactor_vars]

	if(nrow(dat) < ncol(dat)) return("Data has more variables than observations. Please reduce the number of variables.")

	btest <- cortest.bartlett(cor(dat), nrow(dat))
	prefac <- paf(as.matrix(dat))

	return(list(btest = btest, prefac = prefac))
})

summary.factor <- function(result) {

	df <- as.data.frame(result$loadings[])
	# df$Communality <- result$communality
	df$Communality <- 1 - result$uniqueness
	cat("\nFactor loadings matrix\n\n")
	print(df, digits = 3)

	cat("\nFactor scores\n\n")
	scores <- as.data.frame(result$scores)
	print(scores, digits = 3)
	print(str(result))
}

plot.factor <- function(result) {

	df <- round(as.data.frame(result$loadings[]),3)

	if(result$factors > 1) {
		plot(df[,1:2], main = paste("Loadings plot for factors 1 and 2 (Rotation - ",result$rotation,")",sep = ""))
		text(df, adj = c(.5,-.3), labels = rownames(df))
		abline(v = 0, h = 0)
	}

	if(result$factors > 1) {

		x = 1:ncol(df)
		y = 1:nrow(df)

		image(x,y, matrix(0, length(x), length(y)),
	  col='white', xaxt='n', yaxt='n',
 	 	ylim=c(max(y)+0.5, min(y)-0.5), xlab='', ylab='')

		cols = rep('black', ncol(df)*nrow(df))
		pvals <- unlist(df)
		cols[pvals>0.3 & pvals<=0.7] = 'blue'
		centers = expand.grid(y, x)
		text(centers[,2], centers[,1], unlist(df), col=cols)
		grid(length(x),length(y))

		# x = 1:ncol(df)
		# y = 1:nrow(df)
		# image(x, y, t(as.matrix(df)), col = c('blue', 'white', 'red'),
	 #      breaks = c(0, 0.3, 0.7, 1), xaxt='n', yaxt='n',
	 #      ylim=c(max(y)+0.5, min(y)-0.5), xlab='', ylab='')
		# centers = expand.grid(y, x)
		# text(centers[,2], centers[,1], unlist(df))
	}

}

factor <- reactive({
	if(is.null(input$factor_vars) || length(input$factor_vars) < 2) return("Please select two or more variables")

	dat <- getdata()[,input$factor_vars]

	if(nrow(dat) < ncol(dat)) return("Data has more variables than observations. Please reduce the number of variables.")

	nrFac <- as.numeric(input$fac_number)
	if(nrFac > ncol(dat)) {
		cat("The number of factors cannot exceed the number of variables.\n")
		nrFac <- ncol(dat)
	}

	if(input$fac_method == 'PCA') {
		fres <- principal(dat, nfactors=nrFac, rotate=input$fac_rotation, scores=TRUE, oblique.scores=FALSE)
	} else {
		fres <- factanal(dat, nrFac, rotation=input$fac_rotation, scores='regression')
		fres$rotation <- input$fac_rotation
	}

	return(fres)

})



	# if("Save loadings" %in% state$opt) { #{{{
	# 	file.name <- paste('loadings.',state$data,'.xls', sep = "")
	# 	wb <- loadWorkbook(file.name, create = TRUE)

	# 	createSheet(wb, name = 'loadings')
	# 	clearSheet(wb, sheet = 'loadings')

	# 	Variables <- rownames(df)
	# 	df.loadings <- cbind(Variables,df)
	# 	df.loadings[,'Communality'] <- NULL
	# 	writeWorksheet(wb,df.loadings[,-1], sheet = 'loadings', rownames = 'Variables', startRow = 1, startCol = 1)

	# 	max.loading <- createCellStyle(wb)
	# 	setFillPattern(max.loading, fill = XLC$"FILL.SOLID_FOREGROUND")
	# 	setFillForegroundColor(max.loading, color = XLC$"COLOR.SKY_BLUE")
	# 	maxVal <- apply(abs(df.loadings[,-1]),1,max)
	# 	maxValIndex <- which(abs(df.loadings[,-1]) == maxVal, arr.ind = TRUE)
	# 	setCellStyle(wb, sheet = "loadings", row = maxValIndex[,'row']+1, col = maxValIndex[,'col']+1, cellstyle = max.loading)

	# 	df.corr <- data.frame(cor(f.data))
	# 	df.corr <- cbind(Variables,df.corr)
	# 	createSheet(wb, name = 'correlations')
	# 	clearSheet(wb, sheet = 'correlations')
	# 	writeWorksheet(wb, df.corr, sheet = 'correlations', startRow = 1, startCol = 1)
	# 	corr <- createCellStyle(wb)
	# 	setFillPattern(corr, fill = XLC$"FILL.SOLID_FOREGROUND")
	# 	setFillForegroundColor(corr, color = XLC$"COLOR.SKY_BLUE")
	# 	corrIndex <- which(abs(df.corr[,-1]) > .3 & df.corr[,-1] != 1 , arr.ind = TRUE)
	# 	setCellStyle(wb, sheet = "correlations", row = corrIndex[,'row']+1, col = corrIndex[,'col']+1, cellstyle = corr)
	# 	saveWorkbook(wb)

	# 	if(.Platform$OS.type == "unix") {
	# 		execute(paste("browseURL(\"",getwd(),'/',file.name,"\", browser = '/usr/bin/open')",sep=''))
	# 	} else {
	# 		execute(paste("browseURL(\"",getwd(),'/',file.name,"\", browser = NULL)",sep=''))
	# 	}
	# } #}}}
