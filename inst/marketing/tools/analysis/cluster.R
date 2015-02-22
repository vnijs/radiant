###############################################################
# Hierarchical clustering
###############################################################
output$uiHc_vars <- renderUI({

	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
	vars <- varnames()[isNum]
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars,
   	selected = state_multiple("hc_vars", vars),
	  multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
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
	     	selected = state_single("hc_dist", hc_dist_method, "sq.euclidean"),
	     	multiple = FALSE),
	    selectInput("hc_meth", label = "Method:", choices = hc_method,
	     	selected = state_single("hc_meth", hc_method, "ward.D"), multiple = FALSE),
	    conditionalPanel(condition = "input.tabs_hierCluster == 'Plots'",
		    radioButtons(inputId = "hc_plots", label = NULL, hc_plots,
	 	    	selected = state_init("hc_plots", "dendo"),
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

	do.call(hierCluster, hc_inputs())
})

observe({
  if(input$hierClusterReport %>% not_pressed) return()
  isolate({
		updateReport(hc_inputs() %>% clean_args,"hierCluster")
  })
})

hierCluster <- function(dataset, hc_vars,
                        data_filter = "",
                        show_filter = FALSE, 		# remove when recode is complete
                        hc_dist = "sq.euclidian",
                        hc_meth = "ward.D",
                        hc_plots = "dendo",
                        hc_cutoff = 0) {

	getdata_exp(dataset, hc_vars, filt = data_filter) %>%
	  scale %>%
	  { if(hc_dist == "sq.euclidian") {
				dist(., method = "euclidean")^2
			} else {
				dist(., method = hc_dist)
			}
		} %>% hclust(d = ., method = hc_meth) -> hc_out

	environment() %>% as.list %>% set_class(c("hierCluster",class(.)))
}

# list of function arguments
hc_args <- as.list(formals(hierCluster))

# list of function inputs selected by user
hc_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(hc_args))
    hc_args[[i]] <- input[[i]]
  if(!input$show_filter) hc_args$data_filter = ""
  hc_args
})

# main functions called from radiant.R
summary_hierCluster <- function(result = .hierCluster()) {

	cat("Hierarchical cluster analysis\n")
	cat("Data        :", result$dataset, "\n")
	if(result$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", result$data_filter), "\n")
	cat("Variables   :", paste0(result$hc_vars, collapse=", "), "\n")
	cat("Method      :", result$hc_meth, "\n")
	cat("Distance    :", result$hc_dist, "\n")
	cat("Observations:", length(result$hc_out$order), "\n")
	cat("Note        : The main output from this analysis are the plots in the Plots tab.")
}

plots_hierCluster <- function(result = .hierCluster()) {

	result$hc_out$height %<>% { . / max(.) }

	if(result$hc_plots == "dendo") {
		as.dendrogram(result$hc_out) %>%
		{
			if(result$hc_cutoff == 0) {
				plot(., main = "Dendrogram", xlab = '', ylab = 'Heterogeneity')
			} else {
				plot(., ylim = c(result$hc_cutoff,1), leaflab='none',
				     main = "Cutoff Dendrogram", ylab = 'Heterogeneity')
			}
		}
	} else {
		height <- rev(result$hc_out$height[result$hc_out$height > result$hc_cutoff])
		nr_of_clusters <- 1:length(height)
		plot(nr_of_clusters, height, main = "Scree plot", xlab = "Nr of clusters",
			ylab = "Heterogeneity", type = 'b')
	}
}

# library(dplyr)
# library(magrittr)
# source("~/gh/radiant_dev/R/radiant.R")
# data <- "mtcars"
# hc_vars <- c("cyl", "mpg")
# hc_plots <- "dendo"
# result <- hierCluster(data, hc_vars, hc_plots = hc_plots)
# plots_hierCluster(result)

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
  selectInput(inputId = "km_vars", label = "Variables:", choices = vars,
	  selected = state_multiple("km_vars", vars, input$hc_vars),
	  multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
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
			     	selected = state_single("km_dist", hc_dist_method, "sq.euclidian"), multiple = FALSE),
	  			selectInput("km_meth", label = "Method:", choices = hc_method,
			     	selected = state_single("km_meth", hc_method, "ward.D"), multiple = FALSE)
	  		)
	  	),
	  	conditionalPanel(condition = "input.km_hcinit == false",
		    numericInput("km_seed", "Set random seed:", min = 0,
		    	value = state_init('km_seed',1234))
		  ),
	    numericInput("km_nrClus", "Number of clusters:", min = 2,
	    	value = state_init('km_nrClus',2)),

			HTML("<label>Save:</label>"), br(),
      downloadButton("km_save_clusmeans", "Means"),
	    actionButton("km_saveclus", "Membership")
  	),
		helpAndReport('Kmeans cluster analysis','kmeansCluster',inclMD("tools/help/kmeansClustering.md"))
	)
})

output$km_save_clusmeans <- downloadHandler(
  filename = function() { "cluster_kmeans.csv" },
  content = function(file) {
  	summary_kmeansCluster(savemeans = TRUE) %>% write.csv(., file = file)
  }
)

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

	if(input$km_vars %>% not_available)
		return("This analysis requires variables of type numeric or integer.\nIf none are availble please select another dataset.")

	do.call(kmeansCluster, km_inputs())
})

observe({
  if(input$kmeansClusterReport %>% not_pressed) return()
  isolate({

		# extra command to save cluster membership
 		xcmd <- paste0("save_clusters(result)")
		updateReport(km_inputs() %>% clean_args, "kmeansCluster",
		             round(7 * kmeans_plotWidth()/650,2),
		             round(7 * kmeans_plotHeight()/650,2),
		             xcmd = xcmd)
  })
})

kmeansCluster <- function(dataset, km_vars,
                        	data_filter = "",
                        	show_filter = FALSE, 		# remove when recode is complete
                          km_hcinit = TRUE,
                          km_dist = "sq.euclidian",
                          km_meth = "ward.D",
                          km_seed = 1234,
                          km_nrClus = 2) {

	dat <- getdata_exp(dataset, km_vars, filt = data_filter)
	dats <- dat %>% scale

	if(km_hcinit) {
		hinit <- hierCluster(dataset, km_vars, data_filter = data_filter,
		                     show_filter = show_filter, hc_dist = km_dist,
		                     hc_meth = km_meth)

		cvar <- cutree(hinit$hc_out, k = km_nrClus)
		cluscenters <- as.matrix(aggregate(dats, list(cvar),mean)[-1])
		km_out <- kmeans(dats, centers = cluscenters, iter.max = 500)
	} else {
		set.seed(km_seed)
		km_out <- kmeans(dats, centers = km_nrClus, nstart = 10, iter.max = 500)
	}
	rm(dats) 	# don't need scaled version anymore

	nrVars <- length(km_vars)
	plotHeight <- 325 * (1 + floor((nrVars - 1) / 2))
	plotWidth <- 325 * min(nrVars,2)

	environment() %>% as.list %>% set_class(c("kmeansCluster",class(.)))
}

# list of function arguments
km_args <- as.list(formals(kmeansCluster))

# list of function inputs selected by user
km_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(km_args))
    km_args[[i]] <- input[[i]]
  if(!input$show_filter) km_args$data_filter = ""
  km_args
})


summary_kmeansCluster <- function(result = .kmeansCluster(), savemeans = FALSE) {

	nrClus <- result$km_nrClus
	clusNames <- paste("Cluster",1:nrClus)
	result$dat %>%
		mutate(cvar = result$km_out$cluster) %>%
		group_by(cvar) %>%
		summarise_each(funs(mean)) %>%
		select(-cvar) %>%
		round(3) %>%
		set_rownames(clusNames) %>%
		as.data.frame -> cmeans
		if(savemeans) return(cmeans)

	cat("K-means cluster analysis\n")
	cat("Data        :", result$dataset, "\n")
	if(result$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", result$data_filter), "\n")
	cat("Variables   :", paste0(result$km_vars, collapse=", "), "\n")
	if(result$km_hcinit) {
		cat("Method      :", result$km_meth, "\n")
		cat("Distance    :", result$km_dist, "\n")
	}
	cat("Observations:", result$dat %>% nrow, "\n")
	cat("Generated   :", nrClus, "clusters of sizes", paste0(result$km_out$size, collapse=", "),"\n\n")

	cat("Cluster means:\n")
	print(cmeans)

	# percentage of within cluster variance accounted for by each cluster
	cat("\nPercentage of within cluster variance accounted for by each cluster:\n")
	(100 * result$km_out$withinss / result$km_out$tot.withinss) %>%
		round(2) %>%
		sprintf("%.2f",.) %>%
		paste0(.,"%") %>%
		as.data.frame %>%
		set_rownames(clusNames) %>%
		set_colnames("") %>%
		print

	# percentage of between cluster variance versus the total higher is better
	(100 * result$km_out$betweenss / result$km_out$totss) %>% sprintf("%.2f",.) %>%
		paste0("\nBetween cluster variance accounts for ", . , "% of the\ntotal variance in the data (higher is better).") %>%
		cat
}

plots_kmeansCluster <- function(result = .kmeansCluster()) {
	result$dat$clusvar <- as.factor(result$km_out$cluster)

	plots <- list()
	for(var in result$km_vars) {
		plots[[var]] <- ggplot(result$dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=2.5, alpha=.3) +
				labs(y = "") + theme(axis.text.y = element_blank())
	}
	sshh( do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))) )
}

# this function should be exported so it can be called externally
# should not use changedata (reactive)
save_clusters <- function(result) {
	data.frame(as.factor(result$km_out$cluster)) %>%
	changedata(., paste0("kclus",result$km_nrClus))
}

# save cluster membership when action button is pressed
observe({
	if(input$km_saveclus %>% not_pressed) return()
	isolate({
		result <- .kmeansCluster()
		if(is.character(result)) return()
		save_clusters(result)
	})
})
