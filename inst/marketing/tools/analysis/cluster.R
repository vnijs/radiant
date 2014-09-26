###############################################################
# Hierarchical clustering
###############################################################
output$uiHc_vars <- renderUI({

 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, 
   	selected = state_multvar("hc_vars",vars), multiple = TRUE, selectize = FALSE)
})

hc_method <- list("Ward's" = "ward", "Single" = "single", "Complete" = "complete", "Average" = "average", 
	"McQuitty" =  "mcquitty", "Median" = "median", "Centroid" = "centroid")

hc_dist_method <- c("Squared euclidean" = "sq.euclidian", "Euclidian" = "euclidean",
	"Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary", "Minkowski" = "minkowski")

hc_plots <- c("Dendrogram" = "dendo", "Scree plot" = "scree")

output$ui_hierCluster <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiHc_vars"), 
	    selectInput("hc_dist", label = "Distance measure:", choices = hc_dist_method, 
	     	selected = state_init_list("hc_dist","sq.euclidean", hc_dist_method), multiple = FALSE),
	    selectInput("hc_meth", label = "Method:", choices = hc_method, 
	     	selected = state_init_list("hc_meth","ward", hc_method), multiple = FALSE),
	    conditionalPanel(condition = "input.tabs_hierCluster == 'Plots'",
		    radioButtons(inputId = "hc_plots", label = "", hc_plots, 
	 	    	selected = state_init_list("hc_plots","dendo", hc_plots)),
	    	numericInput("hc_cutoff", "Plot cutoff:", min = 0, max = 1, 
	    		value = state_init('hc_cutoff',0), step = .05))
  	),
		helpAndReport('Hierarchical cluster analysis','hierCluster',inclMD("tools/help/hcclustering.md"))
	)
})

output$hierCluster <- renderUI({
	# for input-output
  statTabPanel("Clustser", "Hierarchical", ".hierCluster", "hierCluster")
})

.hierCluster <- reactive({

	# if(is.null(input$hc_vars)) return("Please select one or more variables of type numeric or integer.")
	if(is.null(input$hc_vars)) return("Please select one or more variables of type numeric or integer.")
	ret_text <- "This analysis requires variables of type numeric or integer.\nPlease select another dataset."
	# if(is.null(inChecker(c(input$hc_vars)))) return(ret_text)

	hierCluster(input$datasets, input$hc_vars, input$hc_dist, input$hc_meth, input$hc_plots,
		input$hc_cutoff)
})

observe({
  if(is.null(input$hierClusterReport) || input$hierClusterReport == 0) return()
  isolate({

		inp <- list(input$datasets, input$hc_vars, input$hc_dist, input$hc_meth, input$hc_plots, 
			input$hc_cutoff)
		updateReport(inp,"hierCluster")
  })
})

hierCluster <- function(datasets, hc_vars, hc_dist, hc_meth, hc_plots, hc_cutoff) {

	dat <- na.omit( values[[datasets]][,hc_vars] ) 	# omitting missing values
	dat <- scale(dat) 															# standardizing the data

	if(hc_dist == "sq.euclidian") {
		dist.data <- dist(dat, method = "euclidean")^2
	} else {
		dist.data <- dist(dat, method = hc_dist)
	}

	res <- hclust(d = dist.data, method = hc_meth)

	res$hc_vars <- hc_vars
	res$hc_plots <- hc_plots
	res$hc_cutoff <- hc_cutoff

	return(res)
}

# main functions called from radiant.R
summary_hierCluster <- function(result = .hierCluster()) {
	cat("Variables used:\n",result$hc_vars,"\n")
	print(result)
	cat("Note: The main output from this analysis is shown in the Plots tab.")
}

plots_hierCluster <- function(result = .hierCluster()) {

	max_height <- max(result$height)
	result$height <- result$height / max_height

	if(result$hc_plots == "dendo") {

		dend <- as.dendrogram(result)
		if(result$hc_cutoff == 0) {
			plot(dend, main = "Dendrogram", xlab = '', ylab = 'Heterogeneity')
		} else {
			plot(dend, ylim = c(result$hc_cutoff,1), leaflab='none', main = "Cutoff Dendrogram", 
				ylab = 'Heterogeneity')
		}

	} else {
		height <- rev(result$height[result$height > result$hc_cutoff])
		nr_of_clusters <- 1:length(height)
		plot(nr_of_clusters,height, main = "Scree plot", xlab = "Nr of clusters", 
			ylab = "Heterogeneity", type = 'b')
	}
}

# Could use ggplot2 for dendrogram
# library(ggplot2)
# library(ggdendro)
# hc <- hclust(dist(USArrests))
# hcdata <- dendro_data(hc, type="rectangle")
# ggplot() + 
#     geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
#     geom_text(data=label(hcdata), aes(x=x, y=y, label=label, hjust=0), size=3) +
#     coord_flip() + scale_y_reverse(expand=c(0.2, 0))

# ### demonstrate plotting directly from object class hclust
# ggdendrogram(hc, rotate=FALSE)
# ggdendrogram(hc, rotate=TRUE)
# ### demonstrate converting hclust to dendro using dendro_data first
# hcdata <- dendro_data(hc)
# ggdendrogram(hcdata, rotate=TRUE) + labs(title="Dendrogram in ggplot2")

###############################################################
# Kmeans clustering
###############################################################
output$uiKm_vars <- renderUI({

 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectInput(inputId = "km_vars", label = "Variables:", choices = vars, 
	  selected = state_init_multvar("km_vars",input$hc_vars, vars), multiple = TRUE, selectize = FALSE)
})
  
output$ui_kmeansCluster <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiKm_vars"), 
		  checkboxInput(inputId = "km_hcinit", label = "Initial centers from HC", 
      	value = state_init('km_hcinit',TRUE)),
	  	conditionalPanel(condition = "input.km_hcinit == true",
	  		wellPanel(
		  		selectInput("km_dist", label = "Distance measure:", choices = hc_dist_method, 
			     	selected = state_init_list("km_dist","sq.euclidian", hc_dist_method), multiple = FALSE),
	  			selectInput("km_meth", label = "Method:", choices = hc_method, 
			     	selected = state_init_list("km_meth","ward", hc_method), multiple = FALSE)
	  		)
	  	),
	  	conditionalPanel(condition = "input.km_hcinit == false", 
		    numericInput("km_seed", "Set random seed:", min = 0,
		    	value = state_init('km_seed',1234))
		  ),
	    numericInput("km_nrClus", "Number of clusters:", min = 2, 
	    	value = state_init('km_nrClus',2)),
	    actionButton("km_saveclus", "Save cluster membership")
  	),
		helpAndReport('Kmeans cluster analysis','kmeansCluster',inclMD("tools/help/kmeansClustering.md"))
	)
})

kmeans_plotWidth <- function() {
	result <- .kmeansCluster()
	ifelse(is.list(result), return(result$plotWidth), return(650))
}

kmeans_plotHeight <- function() {
	result <- .kmeansCluster()
	ifelse(is.list(result), return(result$plotHeight), return(650))
}

output$kmeansCluster <- renderUI({
	# for input-output
  statTabPanel("Cluster","K-means",".kmeansCluster", "kmeansCluster", "kmeans_plotWidth", "kmeans_plotHeight")
})

.kmeansCluster <- reactive({

	if(is.null(input$km_vars)) return("Please select one or more variables of type numeric or integer.")
	ret_text <- "This analysis requires variables of type numeric or integer.\nPlease select another dataset."
	# if(is.null(inChecker(c(input$km_vars)))) return(ret_text)

	kmeansCluster(input$datasets, input$km_vars, input$km_hcinit, input$km_dist, input$km_meth, input$km_seed,
		input$km_nrClus)
})

observe({
  if(is.null(input$kmeansClusterReport) || input$kmeansClusterReport == 0) return()
  isolate({
	  inp <- list(input$datasets, input$km_vars, input$km_hcinit, input$km_dist, input$km_meth, input$km_seed,
			input$km_nrClus)

		# extra command to save cluster membership
 		xcmd <- paste0("saveClusters(result)")
		updateReport(inp,"kmeansCluster", round(7 * kmeans_plotWidth()/650,2), round(7 * kmeans_plotHeight()/650,2), xcmd = xcmd)
  })
})

kmeansCluster <- function(datasets, km_vars, km_hcinit, km_dist, km_meth, km_seed, km_nrClus) {

	dat <- na.omit( values[[datasets]][,km_vars] ) 					# omitting missing values
	dat <- scale(dat)

	if(km_hcinit) {
		hinit <- hierCluster(datasets, km_vars, km_dist, km_meth, "", 0)
		clusmem <- cutree(hinit, k = km_nrClus)
		cluscenters <- as.matrix(aggregate(dat,list(clusmem),mean)[-1])
		km_res <- kmeans(na.omit(object = data.frame(dat)), centers = cluscenters, iter.max = 500)
	} else {
		set.seed(km_seed)
		km_res <- kmeans(na.omit(object = data.frame(dat)), centers = km_nrClus, nstart = 10, 
			iter.max = 500)
	}

	nrVars <- length(km_vars)
	km_res$plotHeight <- 325 * (1 + floor((nrVars - 1) / 2))
	km_res$plotWidth <- 325 * min(nrVars,2)

	km_res$datasets <- datasets
	km_res$km_vars <- km_vars
	km_res$km_nrClus <- km_nrClus

	km_res
}

summary_kmeansCluster <- function(result = .kmeansCluster()) {
	nrClus <- result$km_nrClus

	cat("Kmeans clustering with", nrClus, "clusters of sizes", paste0(result$size, collapse=", "),"\n\n")
	cat("Cluster means:\n")
	
	dat <- na.omit( values[[result$datasets]][,result$km_vars, drop = FALSE] ) 					# omitting missing values
	cvar <- as.factor(result$cluster)
	dat <- cbind(cvar,dat)
	cnt <- ddply(dat, c("cvar"), colwise(mean))
	cnt <- cnt[,-1, drop = FALSE]
	colnames(cnt) <- result$km_vars
	clusNames <- paste("Cluster",1:nrClus)
	rownames(cnt) <- clusNames
	print(cnt, digits = 3)

	# percentage of within cluster variance accounted for by each cluster
	perc_within <- 100 * (result$withinss / result$tot.withinss)
	perc_within <- data.frame(paste0(sprintf("%.2f",perc_within),"%"))
	rownames(perc_within) <- clusNames
	colnames(perc_within) <- ""
	cat("\nPercentage of within cluster variance accounted for by each cluster:\n")
	print(perc_within, digits = 1)

	# percentage of between cluster variance versus the total higher is better
	perc_between <- 100*(result$betweenss / result$totss)
	cat(paste0("\nBetween cluster variance accounts for ", sprintf("%.2f",perc_between), "% of the\ntotal variance in the data (higher is better)."))	
}

plots_kmeansCluster <- function(result = .kmeansCluster()) {
	dat <- na.omit( values[[result$datasets]][,result$km_vars, drop = FALSE] ) 					# omitting missing values
	dat$clusvar <- as.factor(result$cluster)

	plots <- list()
	for(var in result$km_vars) {
		plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) +
				labs(y = "") + theme(axis.text.y = element_blank())
	}
	do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2))))
}

saveClusters <- function(result = .kmeansCluster()) {
	clusmem <- data.frame(as.factor(result$cluster))
	changedata(clusmem, paste0("kclus",result$km_nrClus))
}

# save cluster membership when action button is pressed
observe({
	if(is.null(input$km_saveclus) || input$km_saveclus == 0) return()
	isolate({
		result <- .kmeansCluster()
		if(is.character(result)) return()
		saveClusters(result)
	})
})
