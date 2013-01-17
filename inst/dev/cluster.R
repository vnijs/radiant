main.hclustering <- function(state) {
	dat <- getdata()
	dist.data <- as.dist(dist(dat[,state$varinterdep], method = "euclidean")^2)
	hclust(d = dist.data, method= "ward")
}

summary.hclustering <- function(result) {
	# if(is.null(result)) return(cat("Please select one or more variables\n"))
	result
}

plot.hclustering <- function(result) {
	# if(is.null(result)) return()
	# use ggdendro when it gets back on cran
	plot(result, main = "Dendrogram")
}

extra.hclustering <- function(result) {
	data.frame('height' = rev(result$height[result$height > 0]))
}

main.kmeansClustering <- function(state) {
	set.seed(1234)
	dat <- getdata()

	result <- kmeans(na.omit(object = dat[,state$varinterdep]), centers = state$nrClus, nstart = 10, iter.max = 500)

	# callin a reactive function to save clustermembership or not
	clus_members()

	result
}

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
		print(do.call(grid.arrange, plots))
	} else {
			print(ggplot(dat, aes_string(x=input$varinterdep[1], colour='clusvar')) + geom_density(adjust = 1.5))
	}
}

extra.kmeansClustering <- function(result) {
	cat("Under development\n")
}

hclustering <- reactive(function() {
	if(is.null(input$varinterdep)) return("Please select one or more variables")
	main.hclustering(as.list(input))
})

kmeansClustering <- reactive(function() {
	if(is.null(input$varinterdep)) return("Please select one or more variables")
	main.kmeansClustering(as.list(input))
})

# function gets called when button is pressed to save clustermembership 
# to the dataset
clus_members <- reactive(function() {
  if(input$saveclus == 0) return()
  isolate({
	  result <- kmeansClustering()
		var.name <- paste("kclus",input$nrClus,sep="")
		changedata(as.factor(result$cluster), var.name)
 	})
})
