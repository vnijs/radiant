# Pre-factor {{{
makeRadyPreFactorAnalysisDialog <- function() {

	# make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L, 400L)
	dialog$setTitle("Pre-factor diagnostics")

	#add variable selector
	variableSelector <-new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	variableList <- new(VariableListWidget, variableSelector)
	addComponent(dialog,variableSelector,10,430,850,10)
	
	#add a list for the variables
	vars <- new(VariableListWidget,"Variables", variableSelector)
	vars$setTitle("vars")
	addComponent(dialog,vars,100,980,850,480)

	dialog$setCheckFunction(toJava(.preFactorAnalysisCheckFunction))
	dialog$setRunFunction(toJava(.preFactorAnalysisRunFunction))

	### dialog$run()
	return(dialog)
}

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

# Factor analysis {{{
makeRadyFactorAnalysisDialog <- function(){

	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,500L)
	dialog$setTitle("Factor Analysis")
	
	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,450,850,10)
	
	#add a list for the variables
	vars<- new(VariableListWidget,variableSelector)
	vars$setTitle("vars")
	addComponent(dialog, vars,100,980,550,500)
	
	nr.fac <- new(TextAreaWidget,"# of factors")
	nr.fac$setTitle("nr.fac")
	nr.fac$setDefaultModel("")
	addComponent(dialog, nr.fac, 580, 980, 685, 700)

	opt <- new(CheckBoxesWidget,"Options",c("Varimax rotation","Save scores","Save loadings"))
	opt$setTitle("opt")
	opt$setDefaultModel(c("Varimax rotation"))
	addComponent(dialog, opt,700,980,870, 640)

	dialog$setCheckFunction(toJava(.factorAnalysisCheckFunction))
	dialog$setRunFunction(toJava(.factorAnalysisRunFunction))

	### dialog$run()
	return(dialog)
}

.factorAnalysisCheckFunction <- function(state){
	if(length(state$vars) < 2)
		return("Please select at least two variables")
	if(state$nr.fac == "")
		return("Please specify the number of factors to extract")
	return("")
}

.factorAnalysisRunFunction <- function(state){

	cmd <- paste("f.data <-", state$data, "[,", paste("c(\"",paste(state$vars,sep = "", collapse = "\",\""),"\")", sep = ""),"]")
	eval(parse(text = cmd))

	rotate.factors <- "none"
	if( "Varimax rotation" %in% state$opt)
		rotate.factors <- "varimax"
	
	nr.fac <- as.numeric(state$nr.fac)

	f.res <- principal(f.data, nfactors=nr.fac, rotate=rotate.factors, scores=TRUE, oblique.scores=FALSE)
	df <- as.data.frame(f.res$loadings[,colnames(f.res$loadings)])
	### df$Uniqueness <- f.res$uniqueness
	df$Communality <- f.res$communality
	cat("\nFactor loadings matrix\n\n")
	print(df, digits = 3)
	cat("\nFactor scores\n\n")
	if(dim(f.data)[1] > 30) {
		print(f.res$scores[1:30,], digits = 3)
		if("Save scores" %in% state$opt) {
			cat("\nFactor scores have been added to the data-file:",state$data,"\n\n")

		} else {
			cat("\nFor a full view of the Factor scores choose the 'Save scores' option in the Factor analysis dialog.\n\n")
		}
	} else {
		print(f.res$scores, digits = 3)
	}

	if(nr.fac == 2) {

		JavaGD(name="Factor loadings", width=600, height=600, ps=12)
		plot(df[,1:nr.fac], main = paste("Factor loadings (Rotation - ",rotate.factors,")",sep = ""))
		text(df[,1:nr.fac], adj = c(.5,-.3), labels = rownames(df))
		abline(v = 0)
		abline(h = 0)
	}

	if("Save scores" %in% state$opt) {

		eval(parse(text = paste("full.data <-", state$data,"[,]")))
		var.labs <- colnames(full.data)
		var.exists <- TRUE
		var.name <- 'fs'
		i <- 1
		while(var.exists) {
			if(paste(var.name,i,".1",sep = "") %in% var.labs) {
				i <- i + 1; next
			} else {
				for(j in 1:nr.fac) {
					full.data[,paste(var.name,i,".",j,sep = "")] <- f.res$scores[,j]
				}
				var.exists <- FALSE
			}
		}
		eval(parse(text = paste(state$data, " <<- full.data[,]")))
	}

	if("Save loadings" %in% state$opt) { #{{{
		file.name <- paste('loadings.',state$data,'.xls', sep = "")
		wb <- loadWorkbook(file.name, create = TRUE)

		createSheet(wb, name = 'loadings')
		clearSheet(wb, sheet = 'loadings')

		Variables <- rownames(df)
		df.loadings <- cbind(Variables,df)
		df.loadings[,'Communality'] <- NULL
		writeWorksheet(wb,df.loadings[,-1], sheet = 'loadings', rownames = 'Variables', startRow = 1, startCol = 1)

		max.loading <- createCellStyle(wb)
		setFillPattern(max.loading, fill = XLC$"FILL.SOLID_FOREGROUND")
		setFillForegroundColor(max.loading, color = XLC$"COLOR.SKY_BLUE")
		maxVal <- apply(abs(df.loadings[,-1]),1,max)
		maxValIndex <- which(abs(df.loadings[,-1]) == maxVal, arr.ind = TRUE)
		setCellStyle(wb, sheet = "loadings", row = maxValIndex[,'row']+1, col = maxValIndex[,'col']+1, cellstyle = max.loading)

		### size <- dim(df.loadings[,-1])
		### allRows = seq(length = row.below) + 1
		### allCols = seq(length = row.below) + 1
		### nrFormat = createCellStyle(wb)
		### setDataFormat(nrFormat, format = "0.00")

		### setCellStyle(wb, sheet = 'analysis', row = allRows, col = 3, cellstyle = nrFormat)
		### setCellStyle(wb, sheet = 'regression', row = allRows, col = 2, cellstyle = nrFormat)

		### for(col in currencyColumns) {
		  ### setCellStyle(wb, sheet = sheet, row = allRows, col = col, cellstyle = csPercentage)
		### }

		df.corr <- data.frame(cor(f.data))
		df.corr <- cbind(Variables,df.corr)
		createSheet(wb, name = 'correlations')
		clearSheet(wb, sheet = 'correlations')
		writeWorksheet(wb, df.corr, sheet = 'correlations', startRow = 1, startCol = 1)
		corr <- createCellStyle(wb)
		setFillPattern(corr, fill = XLC$"FILL.SOLID_FOREGROUND")
		setFillForegroundColor(corr, color = XLC$"COLOR.SKY_BLUE")
		corrIndex <- which(abs(df.corr[,-1]) > .3 & df.corr[,-1] != 1 , arr.ind = TRUE)
		setCellStyle(wb, sheet = "correlations", row = corrIndex[,'row']+1, col = corrIndex[,'col']+1, cellstyle = corr)

		### allRows = seq(length = row.below) + 1
		### nrFormat = createCellStyle(wb)
		### setDataFormat(nrFormat, format = "0.00")

		### setCellStyle(wb, sheet = 'analysis', row = allRows, col = 3, cellstyle = nrFormat)
		### setCellStyle(wb, sheet = 'regression', row = allRows, col = 2, cellstyle = nrFormat)

		### for(col in currencyColumns) {
		  ### setCellStyle(wb, sheet = sheet, row = allRows, col = col, cellstyle = csPercentage)
		### }

		saveWorkbook(wb)

		if(.Platform$OS.type == "unix") {
			execute(paste("browseURL(\"",getwd(),'/',file.name,"\", browser = '/usr/bin/open')",sep=''))
		} else {
			execute(paste("browseURL(\"",getwd(),'/',file.name,"\", browser = NULL)",sep=''))
		}
	} #}}}

} #}}}
