summary.hclustering <- function(result) {
	result
}

plot.hclustering <- function(result) {
	# use ggdendro when it gets back on cran
	plot(result, main = "Dendrogram")
}

extra.hclustering <- function(result) {
	data.frame('height' = rev(result$height[result$height > 0]))
}

hclustering <- reactive(function() {
	if(is.null(input$varinterdep)) return("Please select one or more variables")
	dist.data <- as.dist(dist(getdata()[,input$varinterdep], method = "euclidean")^2)
	hclust(d = dist.data, method= "ward")
})

observe(function() {
	if(is.null(input$saveclus) || input$saveclus == 0 || input$tool != 'hclustering') return()
	isolate({
		clusmem <- cutree(hclustering(), k = input$nrClus)
		changedata(as.factor(clusmem), paste("hclus",input$nrClus,sep=""))
	})
})

summary.kmeansClustering <- function(result) {
	result
}

plot.kmeansClustering <- function(result) {
	# several things to work on here to clean-up the plots
	dat <- getdata()[,input$varinterdep, drop = FALSE]
	# gg.xlim <- quantile(as.vector(as.matrix(dat)),probs = c(.01,.99))
	dat$clusvar <- as.factor(result$cluster)

	if(ncol(dat) > 2) {
		plots <- list()
		for(var in input$varinterdep) {
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + xlim(gg.xlim[1],gg.xlim[2]) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 1.5) 
		}
		print(do.call(grid.arrange, c(plots, list(ncol = 2))))
	} else {
			print(ggplot(dat, aes_string(x=input$varinterdep[1], colour='clusvar')) + geom_density(adjust = 1.5))
	}
}

extra.kmeansClustering <- function(result) {
	cat("Under development\n")
}

kmeansClustering <- reactive(function() {
	if(is.null(input$varinterdep)) return("Please select one or more variables")
	set.seed(1234)
	kmeans(na.omit(object = getdata()[,input$varinterdep]), centers = input$nrClus, nstart = 10, iter.max = 500)
})

observe(function() {
	if(is.null(input$saveclus) || input$saveclus == 0 || input$tool != 'kmeansClustering') return()
	isolate({
		clusmem <- kmeansClustering()$cluster
		changedata(as.factor(clusmem), paste("kclus",input$nrClus,sep=""))
	})
})
