loadUserData <- function(uFile) {

	filename <- uFile$name
	ext <- file_ext(filename)
	file <- newdata <- sub(paste(".",ext,sep = ""),"",filename)
	ext <- tolower(ext)

	if(ext == 'rda' || ext == 'rdata') {
  	# FIX - avoid loading into global
  	# newdata <- load(inFile$datapath)

		# newdata will hold the name of the object inside the R datafile
	  newdata <- load(uFile$datapath, envir = .GlobalEnv)
	}

	if(datasets[1] == 'choosefile') {
		datasets <<- c(newdata)
	} else {
		datasets <<- unique(c(newdata,datasets))
	}

	if(ext == 'sav') {
		assign(file, read.sav(uFile$datapath), inherits = TRUE)
	} else if(ext == 'dta') {
		assign(file, read.dta(uFile$datapath), inherits = TRUE)
	} else if(ext == 'csv') {
		assign(file, read.csv(uFile$datapath, header = TRUE), inherits = TRUE)
	}
}

loadPackData <- function(pFile) {
	data(list = pFile)

	if(datasets[1] == 'choosefile') {
		datasets <<- c(pFile)
	} else {
		datasets <<- unique(c(pFile,datasets))
	}
}      

summary.dataview <- plot.dataview <- extra.dataview <- function(state) {
	return()
}

main.regression <- function(state) {
	if(is.null(state$dataset)) return()
	formula <- paste(state$var1, "~", paste(state$var2, collapse = " + "))
	# lm(formula, data = get(state$dataset))
	lm(formula, data = getdata())
}

summary.regression <- function(state) {
	if(is.null(state)) return(cat("Please select one or more independent variables\n"))
	summary(state)
}

plot.regression <- function(state) {
	if(is.null(state)) return()
	par(mfrow = c(2,2))
	plot(state, ask = FALSE)
}

extra.regression <- function(state) {
	if(is.null(state)) return()
	if(length(state$coefficients) > 2) {
  	cat("Variance Inflation Factors\n")
  	VIF <- sort(vif(state), decreasing = TRUE)
		data.frame(VIF)
	} else {
  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
	}
}

summary.compareMeans <- function(state) {
	if(is.null(state$var2)) return(cat("Please select one or more variables\n"))
	if(is.null(state$datasets)) return()

	formula <- as.formula(paste(state$var2, "~", state$var1))
	# t.test(formula, data = get(state$dataset))
	t.test(formula, data = getdata())
}

plot.compareMeans <- function(state) {
	if(is.null(state$datasets)) return()
	if(is.null(state$var2)) return()

	# dat <- get(state$datasets)
	dat <- getdata()
	# y <- dat[,state$var2]
	# x <- as.factor(dat[,state$var1])
	# dat <- data.frame(cbind(x,y))
	print(qplot(factor(dat[,state$var1]), dat[,state$var2], data = dat, xlab = state$var1, ylab = state$var2, geom = c("boxplot", "jitter")))
	# print(ggplot(dat, aes_string(x=x, y=state$var2)) + geom_boxplot()) # x must be specified as a factor --> doesn't work with aes_string
	# print(ggplot(dat, aes(x=x, y=y)) + geom_boxplot()) # x must be specified as a factor --> doesn't work with aes_string
}

extra.compareMeans <- function(state) {
	return()
}

main.hclustering <- function(state) {
	# dat <- get(state$dataset)
	dat <- getdata()
	dist.data <- as.dist(dist(dat[,state$varinterdep], method = "euclidean")^2)
	hc <- hclust(d = dist.data, method= "ward")
	hc
}

summary.hclustering <- function(state) {
	if(is.null(state)) return(cat("Please select one or more variables\n"))
		state
}

plot.hclustering <- function(state) {
	# use ggdendro when it gets back on cran
	plot(state, main = "Dendrogram")
}

extra.hclustering <- function(state) {
	height <- data.frame('height' = rev(state$height[state$height > 0]))
	height
}

main.kmeansClustering <- function(state) {
	# if(is.null(state$varinterdep)) return(cat("Please select one or more variables\n"))

	set.seed(1234)
	# dat <- get(state$dataset)
	dat <- getdata()
	nr.clus <- 3.0

	print(state$interdepvar)
	# kmc <- kmeans(na.omit(object = dat[,state$varinterdep]), centers = nr.clus, nstart = 50, iter.max = 500)
	kmc <- kmeans(na.omit(object = dat[,'mpg']), centers = nr.clus, nstart = 50, iter.max = 500)

	# var.name <- paste("kclus#",as.integer(nr.clus),sep="")
	# dat[,var.name] <- as.factor(kmc$cluster)
	# assign(state$dataset, dat, inherit = TRUE)
	# assign(state$dataset, dat)
	# print(varnames())
	# browser();stop()
}

summary.kmeansClustering <- function(state) {
	# state
	kmc <- main.kmeansClustering(as.list(state))
	kmc 
}

plot.kmeansClustering <- function(state) {
	return()
}

extra.kmeansClustering <- function(state) {
	return()
}
