#' K-means cluster analysis
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/kmeans_clus.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param km_vars Vector of variables to include in the analysis
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param km_hc_init Use centers from hier_clus as the starting point
#' @param km_dist Distance for hier_clus
#' @param km_meth Method for hier_clus
#' @param km_seed Random see to use for kmeans if km_hc_init is FALSE
#' @param km_nr_clus Number of clusters to extract
#'
#' @return A list of all variables used in kmeans_clus as an object of class kmeans_clus
#'
#' @examples
#' result <- kmeans_clus("shopping", c("v1:v6"))
#'
#' @seealso \code{\link{summary.kmeans_clus}} to summarize results
#' @seealso \code{\link{plot.kmeans_clus}} to plot results
#' @seealso \code{\link{save_membership}} to add cluster membership to the selected dataset
#'
#' @export
kmeans_clus <- function(dataset, km_vars,
                        data_filter = "",
                        km_hc_init = TRUE,
                        km_dist = "sq.euclidian",
                        km_meth = "ward.D",
                        km_seed = 1234,
                        km_nr_clus = 2) {

	if(km_hc_init) {
		hc_init <- hier_clus(dataset, km_vars, data_filter = data_filter,
		                     hc_dist = km_dist, hc_meth = km_meth)

		clus_var <- cutree(hc_init$hc_out, k = km_nr_clus)
		hc_cent <- c()
		getdata(dataset, km_vars, filt = data_filter) %>%
			mutate(clus_var = clus_var) %>%
			mutate_each(funs(scale)) %T>%
			{
				group_by(., clus_var) %>%
				summarise_each(funs(mean)) %>%
				select(-clus_var) %>%
				as.matrix  ->> hc_cent
			} %>% select(-clus_var) %>%
		kmeans(centers = hc_cent, iter.max = 500) -> km_out
		rm(hc_init, hc_cent)
		# { hc_cent <<- as.matrix(aggregate(., list(clus_var),mean)[-1]) } %>%
	} else {
		set.seed(km_seed)
		getdata(dataset, km_vars, filt = data_filter) %>%
			mutate_each(funs(scale)) %>%
			kmeans(centers = km_nr_clus, nstart = 10, iter.max = 500) -> km_out
	}

	clus_names <- paste("Cluster",1:km_nr_clus)
	getdata(dataset, km_vars, filt = data_filter) %>%
		mutate(clus_var = km_out$cluster) %>%
		group_by(clus_var) %>%
		summarise_each(funs(mean)) %>%
		select(-clus_var) %>%
		round(3) %>%
		set_rownames(clus_names) %>%
		as.data.frame -> clus_means

	nr_obs <- length(km_out$cluster)

	environment() %>% as.list %>% set_class(c("kmeans_clus",class(.)))
}

#' Summary method for kmeans_clus
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/kmeans_clus.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{kmeans_clus}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- kmeans_clus("shopping", km_vars = c("v1:v6"))
#' summary(result)
#'
#' @seealso \code{\link{kmeans_clus}} to generate results
#' @seealso \code{\link{plot.kmeans_clus}} to plot results
#' @seealso \code{\link{save_membership}} to add cluster membership to the selected dataset
#'
#' @export
summary.kmeans_clus <- function(object, ...) {

	cat("K-means cluster analysis\n")
	cat("Data        :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables   :", paste0(object$km_vars, collapse=", "), "\n")
	if(object$km_hc_init) {
		cat("Method      :", object$km_meth, "\n")
		cat("Distance    :", object$km_dist, "\n")
	}
	cat("Observations:", object$nr_obs, "\n")
	cat("Generated   :", object$km_nr_clus, "clusters of sizes", paste0(object$km_out$size, collapse=", "),"\n\n")

	cat("Cluster means:\n")
	print(object$clus_means)

	# percentage of within cluster variance accounted for by each cluster
	cat("\nPercentage of within cluster variance accounted for by each cluster:\n")
	(100 * object$km_out$withinss / object$km_out$tot.withinss) %>%
		round(2) %>%
		sprintf("%.2f",.) %>%
		paste0(.,"%") %>%
		as.data.frame %>%
		set_rownames(object$clus_names) %>%
		set_colnames("") %>%
		print

	# percentage of between cluster variance versus the total higher is better
	(100 * object$km_out$betweenss / object$km_out$totss) %>% sprintf("%.2f",.) %>%
		paste0("\nBetween cluster variance accounts for ", . , "% of the\ntotal variance in the data (higher is better).") %>%
		cat
}

#' Plot method for kmeans_clus
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/kmeans_clus.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{kmeans_clus}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- kmeans_clus("shopping", km_vars = c("v1:v6"))
#' plot(result)
#'
#' @seealso \code{\link{kmeans_clus}} to generate results
#' @seealso \code{\link{summary.kmeans_clus}} to summarize results
#' @seealso \code{\link{save_membership}} to add cluster membership to the selected dataset
#'
#' @export
plot.kmeans_clus <- function(x, ...) {

	object <- x; rm(x)

	# reloading the data
	with(object, getdata(dataset, km_vars, filt = data_filter)) %>%
	mutate(clus_var = as.factor(object$km_out$cluster)) -> dat
	vars <- colnames(dat) %>% .[-length(.)]

	plots <- list()
	for(var in vars) {
		plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clus_var')) +
				geom_density(adjust=2.5, alpha=.3) +
				labs(y = "") + theme(axis.text.y = element_blank())
	}
	sshh( do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))) )
}

#' Add a cluster membership variable to the active dataset
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/kmeans_clus.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{kmeans_clus}}
#'
#' @examples
#' \dontrun{
#' result <- kmeans_clus("shopping", km_vars = c("v1:v6"))
#' save_membership(result)
#' }
#' @seealso \code{\link{kmeans_clus}} to generate results
#' @seealso \code{\link{summary.kmeans_clus}} to summarize results
#' @seealso \code{\link{plot.kmeans_clus}} to plot results
#'
#' @export
save_membership <- function(object) {
	if(object$data_filter != "")
    return("Please deactivate data filters before trying to save cluster membership")
	as.factor(object$km_out$cluster) %>%
	changedata(object$dataset, vars = ., var_names = paste0("kclus",object$km_nr_clus))
}
