###############################
# Conjoint profiles
###############################
output$ui_conjointProfiles <- renderUI({
  list(
  	wellPanel(
			fileInput('uploadAttr', 'Upload attributes:', multiple=FALSE),
	  	downloadButton('downloadProfiles', 'Save profiles')
		),
		helpAndReport('Conjoint profiles','conjointProfiles',inclMD("tools/help/conjoint.md"))
	)
})

output$conjointProfiles <- renderUI({
	# create inputs and outputs - function in radiant.R
  statTabPanel("Conjoint","Create profiles",".conjointProfiles","conjointProfiles")
})

.conjointProfiles <- reactive({
	ret_text <- "Please load a file with attribute information."
	if(is.null(input$uploadAttr)) return(ret_text)
  if(is.null(values[['ca_attr']])) return(ret_text) 

	conjointProfiles(values[['ca_attr']])
})

observe({
  if(is.null(input$conjointProfilesReport) || input$conjointProfilesReport == 0) return()
  isolate({
		inp <- list(values[['ca_attr']])
		updateReport(inp,"conjointProfiles")
  })
})

conjointProfiles <- function(ca_attr = values[['ca_attr']]) {
	for(it in 1:20) {
		cmd <- "ca.attributes <- list(c()"
		for(l in ca_attr) {
			if(l != "") cmd <- paste(cmd, ",", l)
		}
		cmd <- paste(cmd, ")")
		eval(parse(text = cmd))

		ret <- conjointFFD(ca.attributes[-1], it)
		if(!is.null(ret)) break

		# reordering the attributes has an effect on
		# the number of profiles generated
		ca_attr <- sample(ca_attr)
	}

	pro.cor <- cor(data.matrix(ret$frac))

	Profile <- 1:dim(ret$frac)[1]
	FFdesign <- cbind(Profile, ret$frac)

	Profile <- 1:dim(ret$full)[1]
	CFdesign <- cbind(Profile, ret$full)

	return(list(attr = ca_attr, pro.cor = pro.cor, frac = FFdesign, full = CFdesign))
}

summary_conjointProfiles <- function(result = .conjointProfiles()) {
	cat("Attributes and levels:\n")
	cat(paste0(result$attr, collapse="\n"),"\n\n")

	cnames <- strsplit(result$attr, " ")
	cn <- c("Profile")
	for(i in cnames) cn <- c(cn, i[1])

	cat("Fractional factorial design:\n")
	print(result$frac[,cn], row.names = FALSE)

	cat("\nFactorial design correlations:\n")
	print(result$pro.cor, row.names = FALSE)

	cat("\nFull factorial design:\n")
	print(result$full[,cn], row.names = FALSE)
}

plots_conjointProfiles <- function(result = .conjointProfiles()) {
	result <- "Relevant output is in the Summary tab."
	return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
}

# ca_loadUserProfiles <- function(uFile) {
# 	objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(uFile))
# 	values[[objname]] <- read.csv(uFile)
# }

conjointFFD <-function(dat,trial) { #{{{

	experiment = expand.grid(dat)
	nr.levels <- 0
	nr.vars <- length(dat)
	for(i in 1:nr.vars) nr.levels <- nr.levels + length(dat[[i]])
	min.profiles <- nr.levels - nr.vars + 1
	max.profiles <- dim(experiment)[1]
	for (i in min.profiles:max.profiles) {
		set.seed(172110)
		# set.seed(1234)

		design <- optFederov(data = experiment, nTrials=i, maxIteration=1000)

		cor.mat <- cor(data.matrix(design$design))

		# cat('\nEvaluating the',i,'profile design\n\n')
		# print(as.dist(cor.mat), digits = 1)
		# cat('\nD-efficiency:',design$Dea,'\n')

		if(det(cor.mat)==1) break
	}

	nr.profiles <- dim(design$design)[1]
	# cat(paste("\nThe number of profiles selected is equal to",nr.profiles,"\n\n"))
	if(nr.profiles > 24) {
		if(trial < 20) {
			return()
		} else {
			cat(paste("The number of profiles required to generate an orthogonal design 
				is greater than the recommended maximum of 24. Consider reducing the number 
				of attributes and/or levels.\n"))
		}
	}

	return(list(frac = design$design, full = experiment))
}

observe({
	inFile <- input$uploadAttr
  if(!is.null(inFile)) {
    isolate({
      values[['ca_attr']] <- gsub("\"","\'",readLines(inFile$datapath))
    })
  }
})

output$downloadProfiles <- downloadHandler(
	filename = function() { 'conjointProfiles.csv' },
  content = function(file) {
		write.csv(.conjointProfiles()$frac, file, row.names = FALSE)
	}
)

################################################################
# Conjoint regression
################################################################
output$uiCa_var1 <- renderUI({

	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectInput(inputId = "ca_var1", label = "Profile evaluations:", choices = vars, 
   	selected = state_singlevar("ca_var1",vars), multiple = FALSE)
})

output$uiCa_var2 <- renderUI({

  if(is.null(input$ca_var1)) return()
	isFct <- "factor" == getdata_class()
 	vars <- varnames()[isFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "ca_var2", label = "Attributes:", choices = vars, 
  	selected = state_multvar("ca_var2", vars), multiple = TRUE, selectize = FALSE)
})

ca_plots <- list("Part-worths" = "pw", "Importance-weights" = "iw")

output$ui_conjoint <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiCa_var1"),
	    uiOutput("uiCa_var2"),
		  checkboxInput("ca_rev", label = "Reverse evaluation scores", 
		  	value = state_init('ca_rev',FALSE)),
	    conditionalPanel(condition = "input.tabs_conjoint == 'Summary'",
		    checkboxInput(inputId = "ca_vif", label = "Calculate VIF-values", 
			  	value = state_init('ca_vif',FALSE)),
		  	downloadButton('downloadPWs', 'Save PWs')
	  	),
	    conditionalPanel(condition = "input.tabs_conjoint == 'Plots'",
		    checkboxInput(inputId = "ca_scale_plot", label = "Scale PW plots", 
			  	value = state_init('ca_scale_plot',FALSE)),
	      selectInput("ca_plots", "Conjoint plots:", choices = ca_plots, 
	  	  	selected = state_init_list("ca_plots","pw", ca_plots)))
	  ),
		helpAndReport('Conjoint analysis','conjoint',inclMD("tools/help/conjoint.md"))
	)
})

ca_plotWidth <- function() {
	result <- .conjoint()
	ifelse(class(result) == 'lm', return(result$plotWidth), return(650))
}

ca_plotHeight <- function() {
	result <- .conjoint()
	ifelse(class(result) == 'lm', return(result$plotHeight), return(650))
}

output$conjoint <- renderUI({
	# create inputs and outputs - function in radiant.R
  statTabPanel("Conjoint","Conjoint analysis",".conjoint","conjoint", "ca_plotWidth", "ca_plotHeight")
})

.conjoint <- reactive({

	ret_text <- "This analysis requires a dependent variable of type\ninteger or numeric and one or more independent variables or type factor.\nPlease select another dataset."
	if(is.null(input$ca_var1)) return(ret_text)
	isFct <- "factor" == getdata_class()
 	vars <- varnames()[isFct]
  if(length(vars) == 0) return(ret_text)
	if(is.null(input$ca_var2)) return("Please select one or more independent variables of type factor.")
	# if(is.null(inChecker(c(input$ca_var1, input$ca_var2)))) return(ret_text)

	mod <- conjoint(input$datasets, input$ca_var1, input$ca_var2, input$ca_rev, input$ca_vif, 
		input$ca_plots, input$ca_scale_plot)

	if(class(mod) != 'lm') return(mod)

	if(input$ca_plots == 'pw') {
		nrVars <- mod$nrVars
		mod$plotHeight <- 325 * (1 + floor((nrVars - 1) / 2))
		mod$plotWidth <- 325 * min(nrVars,2)
	}

	if(input$ca_plots == 'iw') {
		mod$plotHeight <- 500
		mod$plotWidth <- 500
	}

	mod
})

observe({
  if(is.null(input$conjointReport) || input$conjointReport == 0) return()
  isolate({

		inp <- list(input$datasets, input$ca_var1, input$ca_var2, input$ca_rev, input$ca_vif, 
			input$ca_plots, input$ca_scale_plot)
		updateReport(inp,"conjoint", round(7 * ca_plotWidth()/650,2), round(7 * ca_plotHeight()/650,2))
  })
})

conjoint <- function(datasets, ca_var1, ca_var2, ca_rev, ca_vif, ca_plots, ca_scale_plot) {

	dat <- values[[datasets]]
	formula <- paste(ca_var1, "~", paste(ca_var2, collapse = " + "))

	if(ca_rev) {
		ca_dep <- dat[,ca_var1]
		dat[,ca_var1] <- abs(ca_dep - max(ca_dep)) + 1
	}
	
	mod <- lm(formula, data = dat)

	mod$datasets <- datasets
	mod$ca_var2 <- ca_var2
	mod$nrVars <- length(ca_var2)
	mod$ca_vif <- ca_vif
	mod$ca_plots <- ca_plots
	mod$ca_scale_plot <- ca_scale_plot
	
	return(mod)
}

summary_conjoint <- function(result = .conjoint()) {

	theTable <- ca_theTable(result)
	cat("Conjoint part-worths:\n")
	print(theTable[['PW']], row.names = FALSE)
	cat("\nConjoint importance weights:\n")
	print(theTable[['IW']], row.names = FALSE)
	cat("\nConjoint regression coefficients:\n")

	reg_coeff <- data.frame(round(result$coefficients, 3))
	colnames(reg_coeff) <- "Coefficients"
	print(reg_coeff)

	if(result$ca_vif) {
		cat("\n")
		print(vif.conjoint(result))
	}
}

plots_conjoint <- function(result = .conjoint()) {

	theTable <- ca_theTable(result)
	plot_ylim <- theTable$plot_ylim

	if(result$ca_plots == 'pw') {
		PW.df <- theTable[['PW']]

		plots <- list()
		for(var in result$ca_var2) {
			PW.var <- PW.df[PW.df[,'Attributes'] == var,]

			# setting the levels in the same order as in theTable. Without this
			# ggplot would change the ordering of the price levels
			PW.var$Levels <- factor(PW.var$Levels,levels=PW.var$Levels,ordered=FALSE)

			p <- ggplot(PW.var, aes(x=Levels, y=PW, group = 1)) +
				  geom_line(colour="blue", linetype = 'dotdash', size=.7) + 
	  		  geom_point(colour="blue", size=4, shape=21, fill="white") +
		  	  labs(list(title = paste("Part-worths for", var), x = "")) 
		  	  # theme(axis.text.x = element_text(angle = 45, hjust = 1))

		  if(result$ca_scale_plot) p <- p + ylim(plot_ylim[var,'Min'],plot_ylim[var,'Max'])
			plots[[var]] <- p
		}
		do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2))))
	} else {

		IW.df <- theTable[['IW']]
		p <- ggplot(IW.df, aes(x=Attributes, y=IW, fill = Attributes)) + geom_bar(stat = 'identity', alpha = .5) + 
			theme(legend.position = "none") + labs(list(title = "Importance weights"))
		print(p)
	}
}

ca_theTable <- function(result = .conjoint()) {
	if(is.character(result)) return(list("PW" = "No attributes selected."))

	dat <- values[[result$datasets]]
	attr <- data.frame(dat[ ,result$ca_var2, drop = FALSE])

	isFct <- sapply(attr,is.factor)
	if(sum(isFct) < ncol(attr)) return(list("PW" = "Only factors can be used.", "IW" = "Only factors can be used."))
	levs <- lapply(attr[,isFct, drop = FALSE],levels)
	vars <- colnames(attr)[isFct]

	nlevs <- sapply(levs,length)
	PW.df <- data.frame(rep(vars,nlevs), unlist(levs))
	colnames(PW.df) <- c("Attributes","Levels")
	PW.df$PW <- 0

	# Calculate PW and IW's when interactions are present
	# http://www.slideshare.net/SunnyBose/conjoint-analysis-12090511
	rownames(PW.df) <- paste(PW.df[,'Attributes'], PW.df[,'Levels'], sep = "")

	coeff <- result$coefficients
	BW.reg <- list("Attributes" = "Base utility", "Levels" = "", "PW" = coeff[1])
	PW.df[names(coeff)[-1],'PW'] <- coeff[-1]

	minPW <- PW.df[tapply(1:nrow(PW.df),PW.df$Attributes,function(i) i[which.min(PW.df$PW[i])]),]
	maxPW <- PW.df[tapply(1:nrow(PW.df),PW.df$Attributes,function(i) i[which.max(PW.df$PW[i])]),]
	rownames(minPW) <- minPW$Attributes
	rownames(maxPW) <- maxPW$Attributes

	rangePW <- data.frame(cbind(maxPW[vars,'PW'],minPW[vars,'PW']))
	rangePW$Range <- rangePW[,1] - rangePW[,2]
	colnames(rangePW) <- c("Max","Min","Range")
	rownames(rangePW) <- vars

	# for plot range if standardized
	maxlim <- rangePW[,'Max'] > abs(rangePW[,'Min'])
	maxrange <- max(rangePW[,'Range'])
	plot_ylim <- rangePW[,c('Min','Max')]

	plot_ylim[maxlim,'Max'] <- plot_ylim[maxlim,'Max'] + maxrange - rangePW$Range[maxlim]
	plot_ylim[!maxlim,'Min'] <- plot_ylim[!maxlim,'Min'] - (maxrange - rangePW$Range[!maxlim])
	plot_ylim <- plot_ylim * 1.01 		# expanded max to avoid hiding max points in plot

	IW <- data.frame(vars)
	IW$IW <- rangePW$Range / sum(rangePW$Range)
	colnames(IW) <- c("Attributes","IW")

	PW.df[,'Attributes'] <- as.character(PW.df[,'Attributes'])
	PW.df[,'Levels'] <- as.character(PW.df[,'Levels'])
	PW.df <- rbind(PW.df, c("Base utility","~",coeff[1]))
	PW.df[,'PW'] <- as.numeric(PW.df[,'PW'])

	PW.df[,'PW'] <- round(PW.df[,'PW'],3)
	IW[,'IW'] <- round(IW[,'IW'],3)

	list('PW' = PW.df, 'IW' = IW, 'plot_ylim' = plot_ylim)
}

vif.conjoint <- function(result = .conjoint()) {
	if(result$ca_vif) {
		if(length(result$ca_var2) > 1) {
	  	cat("Variance Inflation Factors\n")

	  	# from vif_regression
	  	VIF <- vif(result)
	  	if(!is.null(dim(VIF))) VIF <- VIF[,'GVIF'] # needed when factors are included
	  	VIF <- sort(VIF, decreasing = TRUE)
	  	ifelse(length(VIF) < 8, return(VIF), return(data.frame(VIF)))
		} else {
	  	cat("Insufficient number of attributes/levels selected to calculate VIF scores\n")
		}
	}
}

output$downloadPWs <- downloadHandler(
	# filename = function() { paste(input$datasets[1], '_PWs.csv', sep='') },
	filename = function() { paste(input$datasets, '_PWs.csv', sep='') },
  content = function(file) {
	  write.csv(ca_theTable()[['PW']], file = file, row.names = FALSE)
  }
)

# code for 'exploded logistic regression' when ranking data is provided
# require(MASS)
# greenwind$rnk <- ordered(as.factor(19 - greenwind$rank))
# levels(greenwind$rnk)
# greenwind$rnk
# head(greenwind)
# greenwind$price <- factor(greenwind$price, levels = c("1.59","1.39","1.19"))
# greenwind$brand <- factor(greenwind$brand, levels = c("Glory","K2R","Bissell"))
# levels(greenwind$brand)
# greenwind$brand

# m <- polr(rnk ~ design + brand + price + ghks + mbg, data = greenwind, Hess = TRUE)
# summary(m)
# cf <- coef(m)
# cf <- cf / max(abs(cf))
# cf

# greenwind$rnk_lm <- 19 - greenwind$rank
# l <- lm(rnk_lm ~ design + brand + price + ghks + mbg, data = greenwind)
# summary(l)
# cl <- coef(l)
# cl <- cl / max(abs(cl))
# cl

# df <- data.frame('cf' = cf, 'cl' = cl[-1])
# print(round(df,1), digits = 1)
