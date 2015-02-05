###############################################################
# Hierarchical clustering
###############################################################
output$uiHc_vars <- renderUI({

	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
	vars <- varnames()[isNum]
  # if(length(vars) == 0) return()
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars,
   	selected = state_multvar("hc_vars",vars), multiple = TRUE, selectize = FALSE)
})

hc_method <- list("Ward's" = "ward.D", "Single" = "single", "Complete" = "complete", "Average" = "average",
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
	     	selected = state_init_list("hc_meth","ward.D", hc_method), multiple = FALSE),
	    conditionalPanel(condition = "input.tabs_hierCluster == 'Plots'",
		    radioButtons(inputId = "hc_plots", label = "", hc_plots,
	 	    	selected = state_init_list("hc_plots","dendo", hc_plots),
	 	    	inline = TRUE),
	    	numericInput("hc_cutoff", "Plot cutoff:", min = 0, max = 1,
	    		value = state_init('hc_cutoff',0), step = .05))
  	),
		helpAndReport('Hierarchical cluster analysis','hierCluster',inclMD("tools/help/hcclustering.md"))
	)
})

output$hierCluster <- renderUI({
	# for input-output
  statTabPanel("Cluster", "Hierarchical", ".hierCluster", "hierCluster")
})

.hierCluster <- reactive({

	if(input$hc_vars %>% not_available)
		return("Please select one or more variables of type numeric or integer.\nIf none are available please choose another dataset.")

	hierCluster(input$dataset, input$hc_vars, input$hc_dist, input$hc_meth, input$hc_plots,
		input$hc_cutoff)
})

observe({
  if(input$hierClusterReport %>% not_pressed) return()
  isolate({

		inp <- list(input$dataset, input$hc_vars, input$hc_dist, input$hc_meth, input$hc_plots,
			input$hc_cutoff)
		updateReport(inp,"hierCluster")
  })
})

hierCluster <- function(dataset, hc_vars, hc_dist, hc_meth, hc_plots, hc_cutoff) {

	dat <- na.omit( r_data[[dataset]][,hc_vars] ) 	# omitting missing values
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
  # if(length(vars) == 0) return()
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
			     	selected = state_init_list("km_meth","ward.D", hc_method), multiple = FALSE)
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
  statTabPanel("Cluster","K-means",".kmeansCluster", "kmeansCluster", "kmeans_plotWidth", "kmeans_plotHeight")
})

.kmeansCluster <- reactive({

	if(input$km_vars %>% not_available) return("This analysis requires variables of type numeric or integer.\nIf none are availble please select another dataset.")
	kmeansCluster(input$dataset, input$km_vars, input$km_hcinit, input$km_dist, input$km_meth, input$km_seed,
		input$km_nrClus)
})

observe({
  if(input$kmeansClusterReport %>% not_pressed) return()
  isolate({
	  inp <- list(input$dataset, input$km_vars, input$km_hcinit, input$km_dist, input$km_meth, input$km_seed,
			input$km_nrClus)

		# extra command to save cluster membership
 		xcmd <- paste0("saveClusters(result)")
		updateReport(inp,"kmeansCluster", round(7 * kmeans_plotWidth()/650,2), round(7 * kmeans_plotHeight()/650,2), xcmd = xcmd)
  })
})

kmeansCluster <- function(dataset, km_vars, km_hcinit, km_dist, km_meth, km_seed, km_nrClus) {

	dat <- na.omit( r_data[[dataset]][,km_vars] ) 					# omitting missing values
	dat <- scale(dat)

	if(km_hcinit) {
		hinit <- hierCluster(dataset, km_vars, km_dist, km_meth, "", 0)
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

	km_res$dataset <- dataset
	km_res$km_vars <- km_vars
	km_res$km_nrClus <- km_nrClus

	km_res
}

summary_kmeansCluster <- function(result = .kmeansCluster()) {

	# print cluster means
	nrClus <- result$km_nrClus
	cat("Kmeans clustering with", nrClus, "clusters of sizes", paste0(result$size, collapse=", "),"\n\n")
	cat("Cluster means:\n")
	clusNames <- paste("Cluster",1:nrClus)
	select_(r_data[[result$dataset]], .dots = result$km_vars) %>%
		na.omit %>%
		mutate(cvar = result$cluster) %>%
		group_by(cvar) %>%
		summarise_each(funs(mean)) %>%
		select(-cvar) %>%
		round(3) %>%
		set_rownames(clusNames) %>%
		as.data.frame %>%
		print

	# percentage of within cluster variance accounted for by each cluster
	cat("\nPercentage of within cluster variance accounted for by each cluster:\n")
	(100 * result$withinss / result$tot.withinss) %>%
		round(2) %>%
		sprintf("%.2f",.) %>%
		paste0(.,"%") %>%
		as.data.frame %>%
		set_rownames(clusNames) %>%
		set_colnames("") %>%
		print

	# percentage of between cluster variance versus the total higher is better
	(100 * result$betweenss / result$totss) %>% sprintf("%.2f",.) %>%
		paste0("\nBetween cluster variance accounts for ", . , "% of the\ntotal variance in the data (higher is better).") %>%
		cat
}

plots_kmeansCluster <- function(result = .kmeansCluster()) {
	dat <- na.omit( r_data[[result$dataset]][,result$km_vars, drop = FALSE] ) 					# omitting missing values
	dat$clusvar <- as.factor(result$cluster)

	plots <- list()
	for(var in result$km_vars) {
		# plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) +
		plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=2.5, alpha=.3) +
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
	if(input$km_saveclus %>% not_pressed) return()
	isolate({
		result <- .kmeansCluster()
		if(is.character(result)) return()
		saveClusters(result)
	})
})
