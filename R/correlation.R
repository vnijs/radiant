#' Calculate correlations for two or more variables
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/correlation.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param cor_var Variables to include in the analysis
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param cor_type Type of correlations to calculate. Options are "pearson", "spearman", and "kendall". "pearson" is the default
#' @param cor_cutoff Show only corrlations larger than the cutoff in absolute value. Default is a cutoff of 0
#'
#' @return A list with all variables defined in the function as an object of class compare_means
#'
#' @examples
#' correlation("diamonds",c("price","carat","clarity"))
#'
#' @seealso \code{\link{summary.correlation}} to summarize results
#' @seealso \code{\link{plot.correlation}} to plot results
#'
#' @importFrom psych corr.test
#'
#' @export
correlation <- function(dataset, cor_var,
                        data_filter = "",
                        cor_type = "pearson",
                        cor_cutoff = 0) {

	# could use data.matrix as the last step in the chain but it seems to be
	# about 25% slower on system time
	getdata(dataset, cor_var, filt = data_filter) %>%
		mutate_each(funs(as.numeric)) -> dat

	plot_height <- plot_width <- 150 * ncol(dat)

  environment() %>% as.list %>% set_class(c("correlation",class(.)))
}

# library(ggplot2)
# library(psych)
# r_data <- list()
# dataset <- "diamonds"
# cor_var <- colnames(diamonds)[1:5]
# r_data$diamonds <- diamonds[1:100,1:5]
# result <- correlation("diamonds",cor_var)

# summary_correlation(result)
# plots_correlation(result)

# system.time(
# 	for(i in 1:100) {
# 		print(i)
# 		correlation("diamonds",colnames(diamonds))
# 	}
# )

#' Summarize method for output from the correlation function. This is a method of class correlation and can be called as summary or summary.correlation
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/correlation.html} for an example in Radiant
#'
#' @examples
#' result <- correlation("titanic", "pclass", "survived")
#' summary(result)
#'
#' @seealso \code{\link{correlation}} to calculate results
#' @seealso \code{\link{plot.correlation}} to plot results
#'
#' @export
summary.correlation <- function(result) {

	# calculate the correlation matrix with p-values using the psych package
	cmat <- suppressWarnings( corr.test(result$dat, method = result$cor_type) )

	cr <- format(round(cmat$r,2))
  cr[abs(cmat$r) < result$cor_cutoff] <- ""
	ltmat <- lower.tri(cr)
  cr[!ltmat] <- ""

	cp <- format(round(cmat$p,2))
  cp[abs(cmat$r) < result$cor_cutoff] <- ""
  cp[!ltmat] <- ""

  cat("Correlation\n")
	cat("Data     :", result$dataset, "\n")
	if(result$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
	cat("Variables:", paste0(result$cor_var, collapse=", "), "\n")
	cat("Null hyp.: variables x and y are not correlated\n")
	cat("Alt. hyp.: variables x and y are correlated\n\n")

	cat("Correlation matrix:\n")
  print(cr, quote = FALSE)
	cat("\np-values:\n")
  print(cp, quote = FALSE)
}

#' Plot method for the correlation function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/correlation.html} for an example in Radiant
#'
#' @examples
#' result <- correlation("titanic", "pclass", "survived", cp_plots = "props")
#' plot(result)
#'
#' @seealso \code{\link{correlation}} to calculate results
#' @seealso \code{\link{summary.correlation}} to summarize results
#'
#' @export
plot.correlation <- function(result) {

	# based mostly on http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=137
	panel.plot <- function(x, y) {
	    usr <- par("usr"); on.exit(par(usr))
	    par(usr = c(0, 1, 0, 1))
	    ct <- cor.test(x,y, method = result$cor_type)
	    sig <- symnum(ct$p.value, corr = FALSE, na = FALSE,
	                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
	                  symbols = c("***", "**", "*", ".", " "))
	    r <- ct$estimate
	    rt <- format(r, digits=2)[1]
	    cex <- 0.5/strwidth(rt)

	    text(.5, .5, rt, cex=cex * abs(r))
	    text(.8, .8, sig, cex=cex, col='blue')
	}
	panel.smooth <- function (x, y) {
    points(x, y)
    # uncomment the lines below if you want linear and loess lines
    # in the scatter plot matrix
		# abline(lm(y~x), col="red")
		# lines(stats::lowess(y~x), col="blue")
	}
	result$dat %>% { if(is.null(.)) result else . } %>%
	pairs(lower.panel=panel.smooth, upper.panel=panel.plot)
}
