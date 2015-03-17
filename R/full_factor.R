#' Factor analysis (PCA)
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param ff_var Variables to include in the analysis
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param ff_meth Factor extraction method to use
#' @param ff_number Number of factors to extract
#' @param ff_rotation Apply varimax rotation or no rotation ("varimax" or "none")
#'
#' @return A list with all variables defined in the function as an object of class full_factor
#'
#' @examples
#' result <- full_factor("diamonds",c("price","carat","table","x","y"))
#' result <- full_factor("diamonds",c("price","carat","table","x","y"), ff_meth = "maxlik")
#' summary(result)
#'
#' @seealso \code{\link{summary.full_factor}} to summarize results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom psych principal
#' @import GPArotation
#'
#' @export
full_factor <- function(dataset, ff_var,
                        data_filter = "",
                        ff_meth = "PCA",
                        ff_number = 2,
                        ff_rotation = "varimax") {

	dat <- getdata(dataset, ff_var, filt = data_filter)

	nrObs <- nrow(dat)
	if(nrObs <= ncol(dat)) {
		ret <- "Data should have more observations than variables.\nPlease reduce the number of variables." %>%
						 set_class(c("full_factor",class(.)))
		return(ret)
	}

	nrFac <- max(1,as.numeric(ff_number))
	if(nrFac > ncol(dat)) {
		cat("The number of factors cannot exceed the number of variables.\n")
		nrFac <- ncol(dat)
	}

	if(ff_meth == 'PCA') {
		fres <- principal(dat, nfactors=nrFac, rotate=ff_rotation, scores=TRUE,
		                  oblique.scores=FALSE)
	} else {
		fres <- factanal(dat, nrFac, rotation=ff_rotation, scores='regression')
	}

	rm(dat)

  environment() %>% as.list %>% set_class(c("full_factor",class(.)))
}

#' Summary method for the full_factor function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{full_factor}}
#' @param ff_cutoff Show only loadings with (absolute) values above ff_cutoff (default = 0)
#' @param ff_sort Sort factor loadings
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- full_factor("diamonds",c("price","carat","depth","table","x"))
#' summary(result)
#' summary(result, ff_cutoff = 0, ff_sort = FALSE)
#' summary(result, ff_cutoff = 0, ff_sort = TRUE)
#' summary(result, ff_cutoff = .5, ff_sort = TRUE)
#'
#' @seealso \code{\link{full_factor}} to calculate results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom psych fa.sort
#'
#' @export
summary.full_factor <- function(object,
                                ff_cutoff = 0,
                                ff_sort = FALSE,
                                ...) {

	if(is.character(object)) return(cat(object))

	cat("Factor analysis\n")
	cat("Data        :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables   :", paste0(object$ff_var, collapse=", "), "\n")
	cat("# factors   :", object$ff_number, "\n")
	cat("Method      :", object$ff_meth, "\n")
	cat("Rotation    :", object$ff_rotation, "\n")
	cat("Observations:", object$nrObs, "\n")

	cat("\nFactor loadings:\n")

	# convert loadings object to data.frame
	lds <- object$fres$loadings %>%
		{ if(ff_sort) fa.sort(.) else . }
	dn <- dimnames(lds)
	lds %<>% matrix(nrow = length(dn[[1]])) %>%
		set_colnames(dn[[2]]) %>% set_rownames(dn[[1]]) %>%
		data.frame

	# show only the loadings > ff_cutoff
  ind <- abs(lds) < ff_cutoff
  print_lds <- round(lds,2)
  print_lds[ind] <- ""
  print(print_lds)

  # fit measures
	colSums(lds^2) %>%
		rbind(., . / length(dn[[1]])) %>%
		rbind(., cumsum(.[2,])) %>%
		round(2) %>%
		set_rownames(c("Eigenvalues","Variance %","Cumulative %")) %>%
		print

	# results from psych
  # object$fres$loadings %>%
		# { if(ff_sort) fa.sort(.) else . } %>%
		# print(cutoff = ff_cutoff, digits = 2)

	cat("\nAttribute communalities:\n")
	data.frame(1 - object$fres$uniqueness) %>%
		set_colnames("") %>% round(2) %>%
		print

	cat("\nFactor scores (max 10 shown):\n")
	as.data.frame(object$fres$scores) %>%
  	round(2) %>%
  	slice(1:min(nrow(.),10)) %>%
  	print(row.names = FALSE)
}

#' Plot method for the full_factor function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{full_factor}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- full_factor("diamonds",c("price","carat","table"))
#' plot(result)
#' result <- full_factor("computer","HighEnd:Business")
#' summary(result)
#'
#' @seealso \code{\link{full_factor}} to calculate results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom psych fa.sort
#'
#' @export
plot.full_factor <- function(x, ...) {

	object <- x; rm(x)

	# when no analysis was conducted (e.g., no variables selected)
	if(is.character(object))
		return(plot(x = 1, type = 'n', main=object, axes = FALSE, xlab = "", ylab = ""))

	if(object$fres$factors < 2) {
		object <- "Plots require two or more factors"
		return(plot(x = 1, type = 'n', main=object, axes = FALSE, xlab = "", ylab = ""))
	}

	df <- round(as.data.frame(object$fres$loadings[]),3)
	rnames <- rownames(df)
	cnames <- colnames(df)
	plots <- list()
	pnr <- 1
	ab_df <- data.frame(a=c(0,0), b=c(1, 0))
	for(i in 1:(length(cnames)-1)) {
		for(j in (i+1):length(cnames)) {
			i_name <- cnames[i]; j_name <- cnames[j]
		  df2 <- cbind(df[, c(i_name,j_name)],rnames)
  		plots[[pnr]] <- ggplot(df2, aes_string(x = i_name, y = j_name, color = 'rnames', label = 'rnames')) +
  										  geom_text() + theme(legend.position = "none") +
  										  xlim(-1,1) + ylim(-1,1) + geom_vline(xintercept = 0) +
  										  geom_hline(yintercept = 0)
  		pnr <- pnr + 1
  	}
	}
	do.call(grid.arrange, c(plots, list(ncol = min(2,length(plots)))))
}

#' Save factor scores to active dataset
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{full_factor}}
#'
#' @examples
#' \dontrun{
#' result <- full_factor("diamonds",c("price","carat","table"))
#' save_factors(result)
#' head(dat)
#' }
#'
#' @export
save_factors <- function(object) {
  if(object$data_filter != "")
    return("Please deactivate data filters before trying to save factor scores")
  object$fres$scores %>% as.data.frame %>%
    changedata(object$dataset, vars = ., var_names = paste0("fac",1:ncol(.)))
}
