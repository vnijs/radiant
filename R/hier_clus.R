#' Hierarchical cluster analysis
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/hier_clus.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param vars Vector of variables to include in the analysis
#' @param distance Distance
#' @param method Method
#' @param max_cases Maximum number of cases allowed (default is 1000)
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in hier_clus as an object of class hier_clus
#'
#' @examples
#' result <- hier_clus("shopping", vars = "v1:v6")
#'
#' @seealso \code{\link{summary.hier_clus}} to summarize results
#' @seealso \code{\link{plot.hier_clus}} to plot results
#'
#' @export
hier_clus <- function(dataset, vars,
                      distance = "sq.euclidian",
                      method = "ward.D",
                      max_cases = 1000,
                      data_filter = "") {

	dat <- getdata(dataset, vars, filt = data_filter)
	if (nrow(dat) > max_cases)
	  return(set_class("The number of cases to cluster exceed the maxium set. Change\nthe number of cases allowed using the 'Max cases' input box.", c("hier_clus","character")))

	dat %>%
	  scale %>%
	  { if (distance == "sq.euclidian") {
				dist(., method = "euclidean")^2
			} else {
				dist(., method = distance)
			}
		} %>% hclust(d = ., method = method) -> hc_out

	if (!is_string(dataset)) dataset <- "-----"

	environment() %>% as.list %>% set_class(c("hier_clus",class(.)))
}

#' Summary method for the hier_clus function
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/hier_clus.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{hier_clus}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- hier_clus("shopping", vars = c("v1:v6"))
#' summary(result)
#'
#' @seealso \code{\link{hier_clus}} to generate results
#' @seealso \code{\link{plot.hier_clus}} to plot results
#'
#' @export
summary.hier_clus <- function(object, ...) {

  if (is.character(object)) return(object)

	cat("Hierarchical cluster analysis\n")
	cat("Data        :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables   :", paste0(object$vars, collapse=", "), "\n")
	cat("Method      :", object$method, "\n")
	cat("Distance    :", object$distance, "\n")
	cat("Observations:", length(object$hc_out$order), "\n")
}

#' Plot method for the hier_clus function
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/hier_clus.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{hier_clus}}
#' @param plots Plots to return. "change" shows the percentage change in within-cluster heterogeneity as respondents are grouped into different number of clusters, "dendro" shows the dendrogram, "scree" shows a scree plot of within-cluster heterogeneity
#' @param cutoff For large datasets plots can take time to render and become hard to interpret. By selection a cutoff point (e.g., 0.05 percent) the initial steps in hierachical cluster analysis are removed from the plot
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- hier_clus("shopping", vars = c("v1:v6"))
#' plot(result, plots = c("change", "scree"), cutoff = .05)
#' plot(result, plots = "dendro", cutoff = 0)
#' shopping %>% hier_clus(vars = c("v1:v6")) %>% plot
#'
#' @seealso \code{\link{hier_clus}} to generate results
#' @seealso \code{\link{summary.hier_clus}} to summarize results
#'
#' @import ggdendro
#'
#' @export
plot.hier_clus <- function(x,
                           plots = c("scree","change"),
                           cutoff = 0.05,
                           shiny = FALSE,
                           ...) {

	object <- x; rm(x)
  if (is.character(object)) return(invisible())

	object$hc_out$height %<>% { . / max(.) }

	plot_list <- list()
	if ("scree" %in% plots) {
		plot_list[["scree"]] <-
			object$hc_out$height[object$hc_out$height > cutoff] %>%
			data.frame(height = ., nr_clus = length(.):1) %>%
			ggplot(aes(x=factor(nr_clus,levels=nr_clus), y=height, group = 1)) +
					  geom_line(colour="blue", linetype = 'dotdash', size=.7) +
		  		  geom_point(colour="blue", size=4, shape=21, fill="white") +
			 		  scale_y_continuous(labels = scales::percent) +
			  	  labs(list(title = paste("Scree plot"), x = "# clusters",
			  	       y = "Within-cluster heterogeneity"))
	}

	if ("change" %in% plots) {
		plot_list[["change"]] <-
			object$hc_out$height[object$hc_out$height > cutoff] %>%
				{ (. - lag(.)) / lag(.) } %>%
				data.frame(bump = ., nr_clus = paste0((length(.)+1):2, "-", length(.):1)) %>%
				na.omit %>%
				ggplot(aes(x=factor(nr_clus, levels = nr_clus), y=bump)) +
					geom_bar(stat = "identity", alpha = .5) +
		 		  scale_y_continuous(labels = scales::percent) +
					labs(list(title = paste("Change in within-cluster heterogeneity"),
					     x = "# clusters", y = "Change in within-cluster heterogeneity"))
	}

	if ("dendro" %in% plots) {

		if (length(object$hc_out$height) < 100) {
			## ggdendro is too slow for larger datasets
			if (cutoff == 0) {
				plot_list[["dendro"]] <-
					ggdendrogram(object$hc_out) + labs(list(title = paste("Dendrogram"), x = "",
					  y = "Within-cluster heterogeneity")) + theme_bw() +
						theme(axis.text.x  = element_text(angle=90, size=6))

			} else {
				plot_list[["dendro"]] <-
					object$hc_out %>% dendro_data(type="rectangle") %>%
						segment %>% filter(y > cutoff) %>%
						ggplot(.) + geom_segment(aes_string(x="x", y="y", xend="xend", yend="yend")) +
						  # scale_y_continuous(limits=c(cutoff, 1)) +
						  labs(list(title = paste("Cutoff dendrogram"), x = "", y = "Within-cluster heterogeneity")) +
	 					  theme_bw() + theme(axis.text.x = element_blank())
			}
		} else {
			## this plot will disappear if the user zooms in/out
			as.dendrogram(object$hc_out) %>%
			{
				if (length(plots) > 1) {
					xlab <- "When the number of observations is larger than 100 only the dendrogram is shown even\n if other types are specified. Call the plot function separately for different plot types."
				} else{
					xlab <- ""
				}
				if (cutoff == 0) {
					plot(., main = "Dendrogram", xlab = xlab, ylab = "Within-cluster heterogeneity")
				} else {
					plot(., ylim = c(cutoff,1), leaflab='none',
					     main = "Cutoff dendrogram", xlab = xlab, ylab = "Within-cluster heterogeneity")
					     # main = "Cutoff dendrogram", xlab = xlab, ylab = "Within cluster heterogeneity", lab = nrprint(height, perc = TRUE, dec = 1))
				}
			}
			return(invisible())
		}
	}

	sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
	 	{ if (shiny) . else print(.) }
}
