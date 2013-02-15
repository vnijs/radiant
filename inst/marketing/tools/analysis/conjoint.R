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
	if(nr.profiles > 24) {
		cat(paste("The number of profiles required to generate an 
		orthogonal design is greater than the recommended maximum of 24. Consider 
		reducing the number of attributes and/or levels.\n\n"))
	}

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

conjointProfiles <- reactive({
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

# getdata <- function(dataset = input$datasets) {
#   values[[dataset]]
# }	

ca_loadUserProfiles <- function(uFile) {

	objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(uFile))
	values[[objname]] <- read.csv(uFile)

}

ca_uploadProfiles <- reactive({
  if(input$ca_upload == 0) return("")
  fpath <- try(file.choose(), silent = TRUE)
  if(is(fpath, 'try-error')) {
  	return("")
  } else {
  	return(fpath)
  }
})

output$ca_downloadProfiles <- downloadHandler(
	filename = function() { paste(input$ca_profiles(),'.csv', sep='') },
  content = function(file) {

	  robj <- input$ca_profiles()
	  assign(robj, getdata())

		write.csv(get(robj), file)
	}
)

output$ca_var1 <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "ca_var1", label = "Profile evaluations:", choices = vars, selected = NULL, multiple = FALSE)
})

output$ca_var2 <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "ca_var2", label = "Attributes:", choices = vars[-which(vars == input$ca_var1)], selected = NULL, multiple = TRUE)
})

plot.conjoint <- function(result) {

	theTable <- ca_theTable(conjoint())

	if(input$ca_plots == 'pw') {
		PW.df <- theTable[['PW']]

		plots <- list()
		for(var in input$ca_var2) {
			PW.var <- PW.df[PW.df[,'Attributes'] == var,]

			# setting the levels in the same order as in theTable. Without this
			# ggplot would change the ordering of the price levels
			PW.var$Levels <- factor(PW.var$Levels,levels=PW.var$Levels,ordered=FALSE)

			# plot.ylim <- c(rangePW[var,'Min'],ceiling(rangePW[maxRangeInd,'Range']))
			# plot.ylim[1] <- floor(plot.ylim[1])
			# nr.ticks.max <- min(8, ceiling(rangePW[maxRangeInd,'Range']))
			# plot.ylim[2] <- ceiling(plot.ylim[1] + plot.ylim[2])
			# plots[[var]] <- ggplot(PW.var, aes_string(x='Levels', y='PW', group = 1)) +
			nPW <- nrow(PW.var)
			plots[[var]] <- ggplot(PW.var, aes(x=Levels, y=PW, group = 1)) +
				  geom_line(colour="blue", linetype = 'dotdash', size=.7) + 
	  		  geom_point(colour="blue", size=4, shape=21, fill="white") +
		  	  labs(list(title = paste("Part-worths for", var), x = ""))
		}
		print(do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))))
	} else {

		IW.df <- theTable[['IW']]
		p <- ggplot(IW.df, aes(x=Attributes, y=IW, fill = Attributes)) + geom_bar(stat = 'identity', alpha = .5) + 
			theme(legend.position = "none") + labs(list(title = "Importance weights"))
		print(p)
	}
}

# create all the interaction terms

# with expand.grid you would get all combinations, including the x^2 ones

# ca_int_vec <- function(ca_vars, nway) {
# 	n <- length(ca_vars)
# 	iway <- c()
# 	for(i in 1:(n-1)) {
# 		for(j in (i+1):n) {
# 			iway <- c(iway, paste(ca_vars[i],ca_vars[j],sep=":"))
# 		}
# 	}
# 	if(n >= 3 && nway == '3way') {
# 		for(i in 1:(n-2)) {
# 			for(j in (i+1):(n-1)) {
# 				for(k in (j+1):n) {
# 					iway <- c(iway, paste(ca_vars[i],ca_vars[j],ca_vars[k],sep=":"))
# 				}
# 			}
# 		}
# 	}
# 	iway
# }

# output$ca_intsel <- renderUI({
#   vars <- input$ca_var2
#   if(is.null(vars) || length(vars) < 2) return()
# 	selectInput("ca_intsel", label = "", choices = ca_int_vec(vars,input$ca_interactions), selected = NULL, multiple = TRUE)
# })

ca_plots <- list("Part-worths" = "pw", "Importance-weights" = "iw")

ui_conjoint <- function() {
  wellPanel(
  	tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")),
    uiOutput("ca_var1"),
    uiOutput("ca_var2"),
	  checkboxInput("ca_rev", label = "Reverse evaluation scores", value = FALSE),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    checkboxInput(inputId = "ca_vif", label = "Calculate VIF-values", value = FALSE),
	  	downloadButton('downloadPWs', 'Save PWs')
  	),
  	# radioButtons("ca_interactions", label = "Interactions:", c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way"), selected = "None"),
    # conditionalPanel(condition = "input.ca_interactions != 'none'",
  	# 	uiOutput("ca_intsel")
  	# ),
  	# br(),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("ca_plots", "Conjoint plots:", choices = ca_plots, selected = 'pw', multiple = FALSE)
    )
  )
}

output$downloadPWs <- downloadHandler(
	filename = function() { paste(input$datasets[1], '_PWs.csv', sep='') },
  content = function(file) {
	  write.csv(ca_theTable(conjoint())[['PW']], file = file, row.names = FALSE)
  }
)

summary.conjoint <- function(result) {

	theTable <- ca_theTable(result)
	cat("Conjoint part-worths:\n")
	print(theTable[['PW']], row.names = FALSE)
	cat("\nConjoint importance weights:\n")
	print(theTable[['IW']], row.names = FALSE)
	cat("\nConjoint regression coefficients:\n")

	reg_coeff <- data.frame(round(result$coefficients, 3))
	colnames(reg_coeff) <- "Coefficients"
	print(reg_coeff)

	if(input$ca_vif) {
		cat("\n")
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

conjoint <- reactive({
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

ca_theTable <- function(result) {
	PW.df <- NULL

	if(is.character(result)) return(list("PW" = "No attributes selected."))

	dat <- getdata()
	attr <- data.frame(dat[ ,input$ca_var2, drop = FALSE])

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

	maxRangeInd <- which.max(rangePW$Range)
	ylim <- rangePW[maxRangeInd,c("Min","Max")]

	IW <- data.frame(vars)
	IW$IW <- rangePW$Range / sum(rangePW$Range)
	colnames(IW) <- c("Attributes","IW")

	PW.df[,'Attributes'] <- as.character(PW.df[,'Attributes'])
	PW.df[,'Levels'] <- as.character(PW.df[,'Levels'])
	PW.df <- rbind(PW.df, c("Base utility","~",coeff[1]))
	PW.df[,'PW'] <- as.numeric(PW.df[,'PW'])

	PW.df[,'PW'] <- round(PW.df[,'PW'],3)
	IW[,'IW'] <- round(IW[,'IW'],3)

	list('PW' = PW.df, 'IW' = IW)
}
