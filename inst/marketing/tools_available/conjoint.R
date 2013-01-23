# Conjoint profiles {{{
conjointFractionalFactorialDesign<-function(data) { #{{{

	test

	experiment = expand.grid(data)
	nr.levels <- 0
	nr.vars <- length(data)
	for(i in 1:nr.vars) nr.levels <- nr.levels + length(data[[i]])
	min.profiles <- nr.levels - nr.vars + 1
	max.profiles <- dim(experiment)[1]
	for (i in min.profiles:max.profiles) {
		set.seed(172110)
		design <- optFederov(data = experiment, nTrials=i, maxIteration=1000)
		cor.mat <- cor(data.matrix(design$design))
		cat('\nEvaluating the',i,'profile design\n\n')
		print(cor.mat, digits = 2)
		cat('\nD-efficiency:',design$Dea,'\n')
		if(det(cor.mat)==1) break
	}

	nr.profiles <- dim(design$design)[1]
	cat(paste("\nThe number of profiles selected is equal to",nr.profiles,"\n\n"))
	if(nr.profiles > 24) cat(paste("The number of profiles required to generate an orthogonal design is greater than the recommended maximum of 24. Consider reducing the number of attributes and/or levels.\n\n"))

	return(list(frac = design$design, full = experiment))
	### return(design$design)
} #}}}

conjointCreateProfiles <- function() { #{{{ 

	attribute.file <- file.choose()

	f <- file(attribute.file, "r", blocking = FALSE)
	ca.attr <- readLines(f)
	close(f)

	cmd <- "ca.attributes <- list(c()"
	for(l in ca.attr) {
		if(l != "")
			cmd <- paste(cmd, ",", l)
	}
	cmd <- paste(cmd, ")")
	eval(parse(text = cmd))

	ret <- conjointFractionalFactorialDesign(ca.attributes[-1])
	pro.files <- sub(".txt$",".xls",attribute.file)

	pro.cor <- cor(data.matrix(ret$frac))

	Profile <- 1:dim(ret$frac)[1]
	FFdesign <- cbind(Profile, ret$frac)

	Profile <- 1:dim(ret$full)[1]
	CFdesign <- cbind(Profile, ret$full)

	wb <- loadWorkbook(pro.files, create = TRUE)

	createSheet(wb, name = c('FF design','correlations','CF design'))
	clearSheet(wb, sheet = c('FF design','correlations','CF design'))

	writeWorksheet(wb, FFdesign, sheet = 'FF design', startRow = 1, startCol = 1)
	writeWorksheet(wb, pro.cor, sheet = 'correlations', rownames = 'Attributes', startRow = 1, startCol = 1)
	writeWorksheet(wb, CFdesign, sheet = 'CF design', startRow = 1, startCol = 1)

	saveWorkbook(wb)

	if(.Platform$OS.type == "unix") {
		execute(paste("browseURL(\"",pro.files,"\", browser = '/usr/bin/open')",sep=''))
	} else {
		execute(paste("browseURL(\"",normalizePath(pro.files,winslash = "/"),"\", browser = NULL)",sep=''))
	}
} #}}}
#}}}

# Conjoint analysis {{{
.conjointCheckFunction <- function(state){ #{{{
	if(length(state$dep) < 1)
		return("Please select a profile evaluation variable")
	if(length(state$indep) < 1)
		return("Please select at least one attribute variable")
	if(length(state$indep) < 2 && "VIF" %in% state$opt)
		return("VIF scores cannot be calculated when the model has only one attribute.\n\n")
	return("")
} #}}}

.conjointRunFunction <- function(state) { #{{{

	cmd <- paste("c.data <-", state$data, "[,", paste("c(\"",paste(c(state$dep,state$indep),sep = "", collapse = "\",\""),"\")", sep = ""),"]\n")
	eval(parse(text = cmd))

	if("Reverse" %in% state$opt) {
		reverse <- paste("Rev.",state$dep,sep = "")
		cn <- colnames(c.data)[-1]
		c.data[,reverse] <- abs(c.data[,state$dep] - max(c.data[,state$dep])) + 1
		state$dep <- reverse
		c.data <- c.data[,c(state$dep,cn)]
	}

	for(var in state$indep)
		c.data[,var] <- as.factor(c.data[,var])

	PW.df <- NULL
	lab <- c()
	for(var in state$indep) {
		n <- nlevels(c.data[,var])
		l <- levels(c.data[,var])
		for(i in 1:n) {
			PW.df <- rbind(PW.df,c(var,l[i]))
			lab <- c(lab,paste(var,l[i],sep="."))
		}
	}

	PW.df <- as.data.frame(PW.df)
	colnames(PW.df) <- c("Attribute","Levels")
	PW.df$PW <- 0
	rownames(PW.df) <- lab

	# add a '.' so the factor level associated with a coeffieicent is clearer.
	cn <- colnames(c.data)
	colnames(c.data)[-1] <- paste(colnames(c.data)[-1],".",sep = "")

	form <-paste(state$dep, " ~ " , colnames(c.data)[2])
	for(var in colnames(c.data)[c(-1,-2)])
		form <- paste(form,"+",var)

	# running the regression
	conjoint <- lm(form, data = c.data)
	colnames(c.data) <- cn
	
	coeff <- as.matrix(conjoint$coefficients)
	colnames(coeff) <- c("Coefficients")
	print(round(coeff,3))

	if("VIF" %in% state$opt)
		print(sort(vif(conjoint), decreasing = T), digits = 3)

	BW.reg <- c("Utility base profile",coeff[1,])
	PW.df[rownames(coeff)[-1],'PW'] <- coeff[-1]
	row.below <- dim(PW.df)[1]+3

	minPW <- PW.df[tapply(1:nrow(PW.df),PW.df$Attribute,function(i) i[which.min(PW.df$PW[i])]),]
	maxPW <- PW.df[tapply(1:nrow(PW.df),PW.df$Attribute,function(i) i[which.max(PW.df$PW[i])]),]
	
	rownames(minPW) <- minPW$Attribute
	rownames(maxPW) <- maxPW$Attribute

	rangePW <- data.frame(cbind(maxPW[state$indep,'PW'],minPW[state$indep,'PW']))
	rangePW$Range <- rangePW[,1] - rangePW[,2]
	colnames(rangePW) <- c("Max","Min","Range")
	rownames(rangePW) <- state$indep

	maxRangeInd <- which.max(rangePW$Range)
	ylim <- rangePW[maxRangeInd,c("Min","Max")]

	IW <- data.frame(state$indep)
	IW$IW <- rangePW$Range / sum(rangePW$Range)
	colnames(IW) <- c("Attribute","IW")

	# writing output to excel
	file.name <- paste('conjoint.',state$data,'.xls', sep = "")
	wb <- loadWorkbook(file.name, create = TRUE)

	createSheet(wb, name = c('data','analysis','regression','plots'))
	clearSheet(wb, sheet = c('data','analysis','regression','plots'))

	writeWorksheet(wb, c.data, sheet = 'data', startRow = 1, startCol = 1)
	writeWorksheet(wb, PW.df, sheet = 'analysis', startRow = 1, startCol = 1)
	mergeCells(wb, sheet = "analysis", reference = paste("A",row.below,":B",row.below,sep = ""))
	writeWorksheet(wb, "Utility base profile", sheet = 'analysis', header = FALSE, startRow = row.below, startCol = 1)
	writeWorksheet(wb, coeff[1], sheet = 'analysis', header = FALSE, startRow = row.below, startCol = 3)
	writeWorksheet(wb, IW, sheet = 'analysis', startRow = 1, startCol = 5)
	writeWorksheet(wb, coeff, sheet = 'regression', rownames = 'Levels', startRow = 1, startCol = 1)

	### setColumnWidth(wb,sheet = 'data',column = 1:20,width = -1)
	### setColumnWidth(wb,sheet = 'analysis',column = 1:6,width = -1)
	### setColumnWidth(wb,sheet = 'regression',column = 1:2,width = -1)

	allRows = seq(length = row.below) + 1
	nrFormat = createCellStyle(wb)
	setDataFormat(nrFormat, format = "0.00")

	setCellStyle(wb, sheet = 'analysis', row = allRows, col = 3, cellstyle = nrFormat)
	setCellStyle(wb, sheet = 'regression', row = allRows, col = 2, cellstyle = nrFormat)

	nrFormat = createCellStyle(wb)
	setDataFormat(nrFormat, format = "0.0%")

	setCellStyle(wb, sheet = 'analysis', row = allRows, col = 6, cellstyle = nrFormat)

	setColumnWidth(wb,sheet = 'data',column = 1:20,width = -1)
	setColumnWidth(wb,sheet = 'analysis',column = 1:6,width = -1)
	setColumnWidth(wb,sheet = 'regression',column = 1:2,width = -1)

	plot.ylim <- c(rangePW[var,'Min'],ceiling(rangePW[maxRangeInd,'Range']))

	plot.row <- 2
	for(var in state$indep) {

		graph.name <- paste("PW.plots.",var,".png",sep = "")
		png(graph.name,width=600, height=600)
		### graph.name <- paste("PW.plot.",var,".pdf",sep = "")
		### pdf(graph.name,width=6, height=6)
		op <- par(mar=c(5, 5, 5, 5))
		plot.ylim <- c(rangePW[var,'Min'],ceiling(rangePW[maxRangeInd,'Range']))
		plot.ylim[1] <- floor(plot.ylim[1])
		nr.ticks.max <- min(8, ceiling(rangePW[maxRangeInd,'Range']))
		plot.ylim[2] <- ceiling(plot.ylim[1] + plot.ylim[2])
		nr.ticks <- round((plot.ylim[2] - plot.ylim[1]) / nr.ticks.max,0)
		plot(round(PW.df[PW.df[,'Attribute'] == var,'PW'],3), main = paste("Part-Worths for ",var),type="l", pch = 20, lty = 1, lwd = 5, col = 'blue', xlab = "", ylab = "Part-Worth", ylim = plot.ylim, axes = F)
		lev <- PW.df[PW.df$Attribute == var,'Levels']
		axis(1, 1:length(lev),lev)
		axis(2, round(seq(plot.ylim[1],plot.ylim[2],by = nr.ticks),1), las = 1) 
		if (!(0 %in% round(plot.ylim,1)))
			abline(0,0)
		box("figure", lwd = 4)
		dev.off()
		par(op)

		### ggsave(graph.name, ggplot(PW.df[PW.df[,'Attribute'] == var,'PW'], aes(lev,PW)) + (xlab = "") + (ylab = "Part-Worth"), width = 3.25, height = 3.25, dip = 1200)
		### ggsave(graph.name, ggplot(PW.df[PW.df[,'Attribute'] == var,'PW'], aes(PW,Levels)) + (xlab = "") + (ylab = "Part-Worth"), width = 3.25, height = 3.25, dip = 1200)

		createName(wb, name = "graph", formula = paste("plots!$B$",plot.row,sep=''), overwrite = TRUE)
		addImage(wb, filename = graph.name, name = "graph", originalSize = TRUE)
		plot.row <- plot.row + 40
	}

	graph.name <- paste("IW.plots.",state$data,".png",sep = "")
	png(graph.name,width=600, height=600)
	### graph.name <- paste("IW.plots.",state$data,".pdf",sep = "")
	### pdf(graph.name,width=6, height=6)
	op <- par(mar=c(5, 7, 5, 5))
	IW <- IW[dim(IW)[1]:1,]		# reversing the rows in IW so the IW plot has the right order
	barplot(IW[,'IW'], main = "Importance weights", horiz = TRUE, names.arg = IW[,'Attribute'], las = 1, col = 'blue')
	box("figure", lwd = 4)
	dev.off()
	par(op)

	createName(wb, name = "graph", formula = "plots!$M$2", overwrite = TRUE)
	addImage(wb, filename = graph.name, name = "graph", originalSize = TRUE)

	saveWorkbook(wb)

	if(.Platform$OS.type == "unix") {
		execute(paste("browseURL(\"",getwd(),'/',file.name,"\", browser = '/usr/bin/open')",sep=''))
	} else {
		execute(paste("browseURL(\"",getwd(),'/',file.name,"\", browser = NULL)",sep=''))
	}

} #}}}

makeRadyConjointDialog <-function() { #{{{

	# make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(550L, 650L)
	dialog$setTitle("Conjoint analysis")

	# add variable selector
	variableSelector <-new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	variableList <- new(VariableListWidget, variableSelector)
	addComponent(dialog, variableSelector,1,450,850,10)	

	# add list for the dependent variable
	dep <- new(SingleVariableWidget, "Profile evaluations", variableSelector)
	dep$setTitle("dep")
	addComponent(dialog, dep, 20 ,1000, 120, 500)

	# add list for the independent variable
	indep <- new(VariableListWidget, "Attributes", variableSelector)
	indep$setTitle("indep")
	### addComponent(dialog, indep, 120 ,1000, 420, 500)
	addComponent(dialog, indep, 120 ,1000, 630, 500)

	### # add list for the independent variable
	### factors <- new(VariableListWidget, "Controls", variableSelector)
	### factors$setTitle("factors")
	### addComponent(dialog, factors, 430 ,1000, 630, 500)

	#options for transforming the variables
	### opt <- new(CheckBoxesWidget,"Options",.jarray(c("Reverse","VIF")))
	opt <- new(CheckBoxesWidget,"Options",.jarray(c("Reverse")))
	opt$setTitle("opt")
	opt$setDefaultModel(c(""))
	addComponent(dialog, opt,640,980,780, 640)

	dialog$setCheckFunction(toJava(.conjointCheckFunction))
	dialog$setRunFunction(toJava(.conjointRunFunction))

	### dialog$run()
	return(dialog)
} #}}}
#}}}
