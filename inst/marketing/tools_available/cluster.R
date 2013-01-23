# variable selection - hclustering
output$hc_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

ui_hclustering <- function() {
  wellPanel(
    uiOutput("hc_vars"), tags$style(type='text/css', "#hc_vars { height: 250px; padding-bottom: 35px;}"),
    selectInput(inputId = "hc_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("hc_saveclus", "Save cluster membership")
  )
}

output$km_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "km_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})
  
ui_kmeansClustering <- function() {
  wellPanel(
    uiOutput("km_vars"), tags$style(type='text/css', "#km_vars { height: 250px; padding-bottom: 35px;}"),
    numericInput("km_seed", "Set random seed:", 1234, min = 0),
    selectInput(inputId = "km_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("km_saveclus", "Save cluster membership")
  )
}

summary.hclustering <- function(result) {
	result
}

plot.hclustering <- function(result) {
	# use ggdendro when it gets back on cran
	par(mfrow = c(2,1))
	plot(result, main = "Dendrogram")
	height = rev(result$height[result$height > 0])
	nr_of_clusters = 1:length(height)
	plot(nr_of_clusters,height, xlab = "Nr of clusters", ylab = "Height", type = 'b')
}

extra.hclustering <- function(result) {
	data.frame('height' = rev(result$height[result$height > 0]))
}

hclustering <- reactive(function() {
	if(is.null(input$hc_vars)) return("Please select one or more variables")
	dist.data <- as.dist(dist(getdata()[,input$hc_vars], method = "euclidean")^2)
	hclust(d = dist.data, method= "ward")
})

observe(function() {
	if(is.null(input$hc_saveclus) || input$hc_saveclus == 0) return()
	isolate({
		clusmem <- cutree(hclustering(), k = input$hc_nrClus)
		changedata(as.factor(clusmem), paste("hclus",input$hc_nrClus,sep=""))
	})
})

summary.kmeansClustering <- function(result) {
	result
}

plot.kmeansClustering <- function(result) {
	# several things to work on here to clean-up the plots
	dat <- getdata()[,input$km_vars, drop = FALSE]
	# gg.xlim <- quantile(as.vector(as.matrix(dat)),probs = c(.01,.99))
	dat$clusvar <- as.factor(result$cluster)

	if(ncol(dat) > 2) {
		plots <- list()
		for(var in input$km_vars) {
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + xlim(gg.xlim[1],gg.xlim[2]) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) 
		}
		print(do.call(grid.arrange, c(plots, list(ncol = 2))))
	} else {
		print(ggplot(dat, aes_string(x=input$km_vars[1], fill='clusvar')) + geom_density(adjust=1.5, alpha=.3))
	}
}

extra.kmeansClustering <- function(result) {
	cat("Under development\n")
}

kmeansClustering <- reactive(function() {
	if(is.null(input$km_vars)) return("Please select one or more variables")
	set.seed(input$km_seed)
	kmeans(na.omit(object = getdata()[,input$km_vars]), centers = input$km_nrClus, nstart = 10, iter.max = 500)
})

observe(function() {
	if(is.null(input$km_saveclus) || input$km_saveclus == 0) return()
	isolate({
		clusmem <- kmeansClustering()$cluster
		changedata(as.factor(clusmem), paste("kclus",input$km_nrClus,sep=""))
	})
})
