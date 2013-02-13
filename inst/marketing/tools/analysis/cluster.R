# variable selection - hclustering
output$hc_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

hc_method <- list("Ward's" = "ward", "Single" = "single", "Complete" = "complete", "Average" = "average", 
	"McQuitty" =  "mcquitty", "Median" = "median", "Centroid" = "centroid")

hc_dist_method <- c("sq.euclidian", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

ui_hclustering <- function() {
  wellPanel(
    uiOutput("hc_vars"), 
    # tags$style(type='text/css', "#hc_vars { height: 250px; padding-bottom: 35px;}"),
    selectInput("hc_dist", label = "Distance measure:", choices = hc_dist_method, selected = hc_dist_method[1], multiple = FALSE),
    selectInput("hc_meth", label = "Method:", choices = hc_method, selected = hc_method[1], multiple = FALSE),
    selectInput("hc_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("hc_saveclus", "Save cluster membership")
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

hclustering <- reactive(function() {
	if(is.null(input$hc_vars)) return("Please select one or more variables")

	# dist.data <- as.dist(dist(getdata()[,input$hc_vars], method = "euclidean")^2)
	dat <- getdata()[,input$hc_vars]
	if(input$hc_dist == "sq.euclidian") {
		dist.data <- dist(dat, method = "euclidean")^2
	} else {
		dist.data <- dist(dat, method = input$hc_dist)
	}
	hclust(d = dist.data, method= input$hc_meth)
})

observe(function() {
	if(is.null(input$hc_saveclus) || input$hc_saveclus == 0) return()
	isolate({
		clusmem <- cutree(hclustering(), k = input$hc_nrClus)
		changedata(as.factor(clusmem), paste("hclus",input$hc_nrClus,sep=""))
	})
})

output$km_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "km_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})
  
ui_kmeansClustering <- function() {
  wellPanel(
    uiOutput("km_vars"), 
    # tags$style(type='text/css', "#km_vars { height: 250px; padding-bottom: 35px;}"),
	  checkboxInput(inputId = "km_hcinit", label = "Initial centers from HC", value = TRUE),
  	# conditionalPanel(condition = "input.km_hcinit == 'TRUE'",
  	conditionalPanel(condition = "input.km_hcinit == true",
  		wellPanel(
	  		selectInput("km_dist", label = "Distance measure:", choices = hc_dist_method, selected = hc_dist_method[1], multiple = FALSE),
  			selectInput("km_meth", label = "Method:", choices = hc_method, selected = hc_method[1], multiple = FALSE)
  		)
  	),
  	conditionalPanel(condition = "input.km_hcinit == false", 
	    numericInput("km_seed", "Set random seed:", 1234, min = 0)
	  ),
    selectInput(inputId = "km_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("km_saveclus", "Save cluster membership")
  )
}

summary.kmeansClustering <- function(result) {
	result$cluster = NULL
	result
}

plot.kmeansClustering <- function(result) {
	# several things to work on here to clean-up the plots
	dat <- getdata()[,input$km_vars, drop = FALSE]
	# gg.xlim <- quantile(as.vector(as.matrix(dat)),probs = c(.01,.99))
	dat$clusvar <- as.factor(result$cluster)

		plots <- list()
		for(var in input$km_vars) {
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + xlim(gg.xlim[1],gg.xlim[2]) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) 
		}
		print(do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))))
}

hinitclustering <- reactive(function() {
	if(is.null(input$km_vars)) return("Please select one or more variables")
	dat <- getdata()[,input$km_vars]
	if(input$km_dist == "sq.euclidian") {
		dist.data <- dist(dat, method = "euclidean")^2
	} else {
		dist.data <- dist(dat, method = input$km_dist)
	}
	hclust(d = dist.data, method= input$km_meth)
})

kmeansClustering <- reactive(function() {
	if(is.null(input$km_vars)) return("Please select one or more variables")
	set.seed(input$km_seed)
	dat <- getdata()[,input$km_vars]
	# dat <- lapply(dat,as.numeric) 	# has strange effect
	if(input$km_hcinit) {
		clusmem <- cutree(hinitclustering(), k = input$km_nrClus)
		cluscenters <- as.matrix(aggregate(dat,list(clusmem),mean)[-1])
		kmeans(na.omit(object = data.frame(dat)), centers = cluscenters, iter.max = 500)
	} else {
		kmeans(na.omit(object = data.frame(dat)), centers = input$km_nrClus, nstart = 10, iter.max = 500)
	}
})

observe(function() {
	if(is.null(input$km_saveclus) || input$km_saveclus == 0) return()
	isolate({
		clusmem <- kmeansClustering()$cluster
		changedata(as.factor(clusmem), paste("kclus",input$km_nrClus,sep=""))
	})
})
