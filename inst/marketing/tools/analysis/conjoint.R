conjointFFD <-function(dat) { #{{{

	experiment = expand.grid(dat)
	nr.levels <- 0
	nr.vars <- length(dat)
	for(i in 1:nr.vars) nr.levels <- nr.levels + length(dat[[i]])
	min.profiles <- nr.levels - nr.vars + 1
	max.profiles <- dim(experiment)[1]
	for (i in min.profiles:max.profiles) {
		# set.seed(172110)
		design <- optFederov(data = experiment, nTrials=i, maxIteration=1000)
		# cor.mat <- cor(data.matrix(design$design))
		cor.mat <- cor(as.matrix(design$design))
		cat('\nEvaluating the',i,'profile design\n\n')
		print(as.dist(cor.mat), digits = 1)
		cat('\nD-efficiency:',design$Dea,'\n')
		if(det(cor.mat)==1) break
	}

	nr.profiles <- dim(design$design)[1]
	cat(paste("\nThe number of profiles selected is equal to",nr.profiles,"\n\n"))
	if(nr.profiles > 24) cat(paste("The number of profiles required to generate an orthogonal design is greater than the recommended maximum of 24. Consider reducing the number of attributes and/or levels.\n\n"))

	return(list(frac = design$design, full = experiment))
}

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
	# pro.files <- sub(".txt$",".xls",attribute.file)

	pro.cor <- cor(data.matrix(ret$frac))

	Profile <- 1:dim(ret$frac)[1]
	FFdesign <- cbind(Profile, ret$frac)

	Profile <- 1:dim(ret$full)[1]
	CFdesign <- cbind(Profile, ret$full)

}









output$ca_var1 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "ca_var1", label = "Profile evaluations:", choices = vars, selected = NULL, multiple = FALSE)
})

output$ca_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "ca_var2", label = "Attributes:", choices = vars[-which(vars == input$ca_var1)], selected = NULL, multiple = TRUE)
})

plot.conjoint <- function(result) {
	# mod <- fortify(result)
	# return(coefplot(result, xlab="", ylab="", main="Coefficient plot", col.pts="blue", CI=2))
	plot(1:10)
}

# create all the interaction terms
ca_int_vec <- function(ca_vars, nway) {
	n <- length(ca_vars)
	iway <- c()
	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			iway <- c(iway, paste(ca_vars[i],ca_vars[j],sep=":"))
		}
	}
	if(n >= 3 && nway == '3way') {
		for(i in 1:(n-2)) {
			for(j in (i+1):(n-1)) {
				for(k in (j+1):n) {
					iway <- c(iway, paste(ca_vars[i],ca_vars[j],ca_vars[k],sep=":"))
				}
			}
		}
	}
	iway
}

output$ca_intsel <- reactiveUI(function() {
  vars <- input$ca_var2
  if(is.null(vars) || length(vars) < 2) return()
	selectInput("ca_intsel", label = "", choices = ca_int_vec(vars,input$ca_interactions), selected = NULL, multiple = TRUE)
})

ca_plots <- list("Part-worths" = "pw", "Importance-weights" = "iw")

ui_conjoint <- function() {
  wellPanel(
  	tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")),
    uiOutput("ca_var1"),
    uiOutput("ca_var2"),
	  checkboxInput("ca_rev", label = "Reverse evaluation scores", value = FALSE),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    checkboxInput(inputId = "ca_vif", label = "Calculate VIF-values", value = FALSE)
  	),
  	radioButtons("ca_interactions", label = "Interactions:", c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way"), selected = "None"),
    conditionalPanel(condition = "input.ca_interactions != 'none'",
  		uiOutput("ca_intsel")
  	),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("ca_plots", "Conjoint plots:", choices = ca_plots, selected = 'pw', multiple = FALSE)
    )
  )
}

ca_The_table <- function() {
	PW.df <- NULL
	levs <- unlist(lapply(movie,levels))
	length(levs)
	levs 
	
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
}

summary.conjoint <- function(result) {
	print(summary(result), digits = 4)

	if(input$ca_vif) {
		print(vif.conjoint(result))
	}
}

vif.conjoint <- function(result) {
	if(input$ca_vif) {
		if(length(input$ca_var2) > 1) {
	  	cat("Variance Inflation Factors\n")

	  	VIF <- as.matrix(vif(result))[,1]
	  	VIF <- sort(VIF, decreasing = TRUE)
			t(data.frame(VIF))
		} else {
	  	cat("Insufficient number of attributes/levels selected to calculate VIF scores\n")
		}
	}
}

conjoint <- reactive(function() {
	vars <- input$ca_var2
	if(is.null(vars)) return("Please select one or more attributes")
	if(!is.null(input$ca_intsel) && input$ca_interactions != 'none') vars <- c(vars,input$ca_intsel)

	formula <- paste(input$ca_var1, "~", paste(vars, collapse = " + "))
	dat <- getdata()

	if(input$ca_rev) {
		ca_dep <- dat[,input$ca_var1]
		dat[,input$ca_var1] <- abs(ca_dep - max(ca_dep)) + 1
	}
	mod <- lm(formula, data = dat)
	mod

})

old.conjoint <- function() {

	# if("Reverse" %in% state$opt) {
	# 	reverse <- paste("Rev.",state$dep,sep = "")
	# 	cn <- colnames(c.data)[-1]
	# 	c.data[,reverse] <- abs(c.data[,state$dep] - max(c.data[,state$dep])) + 1
	# 	state$dep <- reverse
	# 	c.data <- c.data[,c(state$dep,cn)]
	# }

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
}