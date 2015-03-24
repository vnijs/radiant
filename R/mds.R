#' (Dis)similarity based brand maps (MDS)
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/mds.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param mds_id1 A character variable or factor with unique entries
#' @param mds_id2 A character variable or factor with unique entries
#' @param mds_dis A numeric measure of brand dissimilarity
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param mds_method Apply metric or non-metric MDS
#' @param mds_dim_number Number of dimensions
#'
#' @return A list of all variables defined in the function as an object of class mds
#'
#' @examples
#' result <- mds("city","from","to","distance")
#' result <- mds("diamonds","clarity","cut","price")
#' summary(result)
#'
#' @seealso \code{\link{summary.mds}} to summarize results
#' @seealso \code{\link{plot.mds}} to plot results
#'
#' @importFrom MASS isoMDS
#'
#' @export
mds <- function(dataset, mds_id1, mds_id2, mds_dis,
                data_filter = "",
                mds_method = "metric",
                mds_dim_number = 2) {

	mds_dim_number <- as.numeric(mds_dim_number)
	dat <- getdata(dataset, c(mds_id1, mds_id2, mds_dis), filt = data_filter)

	dis <- dat[,mds_dis]
	id1 <- dat[,mds_id1] %>% as.character
	id2 <- dat[,mds_id2] %>% as.character
	rm(dat)

	# ids
	lab <- unique(c(id1,id2))
	nrLev <- length(lab)

	lower <- (nrLev * (nrLev -1)) / 2
	nrObs <- length(dis)

	# setup the distance matrix
	mds_dis_mat <- diag(nrLev)
	if(lower == nrObs) {
		mds_dis_mat[lower.tri(mds_dis_mat, diag = FALSE)] <- dis
	} else if((lower + nrLev) == nrObs) {
		mds_dis_mat[lower.tri(mds_dis_mat, diag = TRUE)] <- dis
	} else {
		# return("Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset." %>% set_class(c("mds",class(.))))
		return("Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset.")
	}

	mds_dis_mat %<>% set_rownames(lab) %>%
		set_colnames(lab) %>%
		as.dist

	# Alternative method, metaMDS - requires vegan
	# res <- suppressWarnings(metaMDS(mds_dis_mat, k = mds_dim_number, trymax = 500))
	# if(res$converged == FALSE) return("The MDS algorithm did not converge. Please try again.")

	set.seed(1234)
	res <- MASS::isoMDS(mds_dis_mat, k = mds_dim_number, trace = FALSE)
	res$stress <- res$stress / 100

	if(mds_method == "metric") {
		res$points <- cmdscale(mds_dis_mat, k = mds_dim_number)
		# Using R^2
		# res$stress <- sqrt(1 - cor(dist(res$points),mds_dis_mat)^2) * 100
		# Using standard Kruskal formula for metric MDS
		res$stress	<- { sum((dist(res$points) - mds_dis_mat)^2) / sum(mds_dis_mat^2) } %>%
											 sqrt
	}

	environment() %>% as.list %>% set_class(c("mds",class(.)))
}

#' Summary method for the mds function
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/mds.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{mds}}
#' @param mds_round Rounding to use for output (default = 0). +1 used for coordinates. +2 used for stress measure. Not currently accessible in Radiant
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- mds("city","from","to","distance")
#' summary(result)
#' summary(result, mds_round = 2)
#'
#' @seealso \code{\link{mds}} to calculate results
#' @seealso \code{\link{plot.mds}} to plot results
#'
#' @export
summary.mds <- function(object, mds_round = 1, ...) {

	if(is.character(object)) return(object)

	cat("(Dis)similarity based brand map (MDS)\n")
	cat("Data        :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables   :", paste0(c(object$mds_id1, object$mds_id2, object$mds_dis), collapse=", "), "\n")
	cat("# dimensions:", object$mds_dim_number, "\n")
	meth <- if(object$mds_method == "metric") "Non-metric" else "Metric"
	cat("Method      :", meth, "\n")
	cat("Observations:", object$nrObs, "\n")

	cat("\nOriginal distance data:\n")
	object$mds_dis_mat %>% round(mds_round) %>% print

	cat("\nRecovered distance data:\n")
	object$res$points %>% dist %>% round(mds_round) %>% print

	cat("\nCoordinates:\n")
	object$res$points %>% round(mds_round + 1) %>%
	 set_colnames({paste("Dim ", 1:ncol(.))}) %>%
	 print

	cat("\nStress:", object$res$stress %>% round(mds_round + 2))
}

#' Plot method for the mds function
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/mds.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{mds}}
#' @param mds_rev_dim Flip the axes in plots
#' @param mds_fontsz Font size to use in plots
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- mds("city","from","to","distance")
#' plot(result)
#' plot(result, mds_rev_dim = 1:2)
#' plot(result, mds_rev_dim = 1:2, mds_fontsz = 2)
#'
#' @seealso \code{\link{mds}} to calculate results
#' @seealso \code{\link{summary.mds}} to plot results
#'
#' @importFrom wordcloud textplot
#'
#' @export
plot.mds <- function(x,
                     mds_rev_dim = "",
                     mds_fontsz = 1.3,
                     ...) {

	object <- x; rm(x)

	# set extremes for plot
	lim <- max(abs(object$res$points))

	# set plot space
	if(object$mds_dim_number == 3) {
		op <- par(mfrow=c(3,1))
		mds_fontsz <- mds_fontsz + .6
	} else {
		op <- par(mfrow=c(1,1))
	}

	# reverse selected dimensions
	if(!is.null(mds_rev_dim) && mds_rev_dim != "") {
		as.numeric(mds_rev_dim) %>%
			{ object$res$points[,.] <<- -1 * object$res$points[,.] }
	}

	# plot maps
	for(i in 1:(object$mds_dim_number-1)) {
		for(j in (i+1):object$mds_dim_number) {
			plot(c(-lim,lim),type = "n",xlab='', ylab='', axes = F, asp = 1,
			     yaxt = 'n', xaxt = 'n', ylim=c(-lim, lim), xlim=c(-lim,lim))
			title(paste("Dimension",i,"vs Dimension",j), cex.main = mds_fontsz)
			points(object$res$points[,i], object$res$points[,j], pch = 16, cex = .6)

			# text(object$res$points[,i], object$res$points[,j], object$lab,
			#      col=rainbow(object$nrLev,start=.6,end=.1), adj = c(0.4,-.4))
			wordcloud::textplot(object$res$points[,i], object$res$points[,j]+(.04*lim),
			                    object$lab, col=rainbow(object$nrLev,start=.6,end=.1),
			                    cex = mds_fontsz, new = FALSE)
			abline(v=0, h=0)
		}
	}
	par(op)
}
