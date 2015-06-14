#' Factor analysis (PCA)
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param vars Variables to include in the analysis
#' @param method Factor extraction method to use
#' @param nr_fact Number of factors to extract
#' @param rotation Apply varimax rotation or no rotation ("varimax" or "none")
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in the function as an object of class full_factor
#'
#' @examples
#' result <- full_factor("diamonds",c("price","carat","table","x","y"))
#' result <- full_factor("diamonds",c("price","carat","table","x","y"), method = "maxlik")
#' result <- diamonds %>% full_factor(c("price","carat","table","x","y"), method = "maxlik")
#'
#' @seealso \code{\link{summary.full_factor}} to summarize results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom psych principal
#' @import GPArotation
#'
#' @export
full_factor <- function(dataset, vars,
                        method = "PCA",
                        nr_fact = 2,
                        rotation = "varimax",
                        data_filter = "") {

	dat <- getdata(dataset, vars, filt = data_filter)
	if (!is_string(dataset)) dataset <- "-----"

	nrObs <- nrow(dat)
	if (nrObs <= ncol(dat)) {
		ret <- "Data should have more observations than variables.\nPlease reduce the number of variables." %>%
						 set_class(c("full_factor",class(.)))
		return(ret)
	}

	nrFac <- max(1,as.numeric(nr_fact))
	if (nrFac > ncol(dat)) {
		cat("The number of factors cannot exceed the number of variables.\n")
		nrFac <- ncol(dat)
	}

	if (method == "PCA") {
		fres <- principal(dat, nfactors = nrFac, rotate = rotation, scores = TRUE,
		                  oblique.scores = FALSE)
	} else {
		fres <- factanal(dat, nrFac, rotation = rotation, scores = "regression")
	}

	## convert loadings object to data.frame
	floadings <- fres$loadings %>%
	         {dn <- dimnames(.)
	          matrix(., nrow = length(dn[[1]])) %>%
	          set_colnames(., dn[[2]]) %>%
	          set_rownames(., dn[[1]]) %>%
	          data.frame}

	rm(dat)

  environment() %>% as.list %>% set_class(c("full_factor",class(.)))
}

#' Summary method for the full_factor function
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{full_factor}}
#' @param cutoff Show only loadings with (absolute) values above cutoff (default = 0)
#' @param fsort Sort factor loadings
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- full_factor("diamonds",c("price","carat","depth","table","x"))
#' summary(result)
#' summary(result, cutoff = 0, fsort = FALSE)
#' summary(result, cutoff = 0, fsort = TRUE)
#' summary(result, cutoff = .5, fsort = TRUE)
#' diamonds %>% full_factor(c("price","carat","depth","table","x")) %>% summary
#' diamonds %>% full_factor(c("price","carat","depth","table","x")) %>% summary(cutoff = .5)
#'
#' @seealso \code{\link{full_factor}} to calculate results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom psych fa.sort
#'
#' @export
summary.full_factor <- function(object,
                                cutoff = 0,
                                fsort = FALSE,
                                ...) {

	if (is.character(object)) return(cat(object))

	cat("Factor analysis\n")
	cat("Data        :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables   :", paste0(object$vars, collapse = ", "), "\n")
	cat("# factors   :", object$nr_fact, "\n")
	cat("Method      :", object$method, "\n")
	cat("Rotation    :", object$rotation, "\n")
	cat("Observations:", object$nrObs, "\n")

	cat("\nFactor loadings:\n")

	## show only the loadings > cutoff
  with(object, clean_loadings(floadings, cutoff, fsort, 2)) %>% print

  ## fit measures
	colSums(object$floadings^2) %>%
		rbind(., . / nrow(object$floadings)) %>%
		rbind(., cumsum(.[2,])) %>%
		round(2) %>%
		set_rownames(c("Eigenvalues","Variance %","Cumulative %")) %>%
		print

	# results from psych - uncomment to validate results
  # object$fres$loadings %>%
		# { if (fsort) fa.sort(.) else . } %>%
		# print(cutoff = cutoff, digits = 2)

	cat("\nAttribute communalities:\n")
	data.frame(1 - object$fres$uniqueness) %>%
		set_colnames("") %>% round(2) %>%
		print

	cat("\nFactor scores (max 10 shown):\n")
	as.data.frame(object$fres$scores) %>%
  	round(2) %>%
  	slice(1:min(nrow(.), 10)) %>%
  	print(row.names = FALSE)
}

#' Plot method for the full_factor function
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{full_factor}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- full_factor("diamonds",c("price","carat","table"))
#' plot(result)
#' result <- full_factor("computer","high_end:business")
#' summary(result)
#'
#' @seealso \code{\link{full_factor}} to calculate results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom psych fa.sort
#'
#' @export
plot.full_factor <- function(x,
                             shiny = FALSE,
                             ...) {

	object <- x; rm(x)

	## when no analysis was conducted (e.g., no variables selected)
	if (is.character(object))
		return(plot(x = 1, type = 'n', main = object, axes = FALSE, xlab = "", ylab = ""))

	if (object$fres$factors < 2) {
		object <- "Plots require two or more factors"
		return(plot(x = 1, type = 'n', main = object, axes = FALSE, xlab = "", ylab = ""))
	}

	df <- object$floadings
	rnames <- rownames(df)
	cnames <- colnames(df)
	plots <- list()
	pnr <- 1
	ab_df <- data.frame(a = c(0,0), b = c(1, 0))
	for (i in 1:(length(cnames) - 1)) {
		for (j in (i + 1):length(cnames)) {
			i_name <- cnames[i]; j_name <- cnames[j]
		  df2 <- cbind(df[, c(i_name,j_name)],rnames)
  		plots[[pnr]] <- ggplot(df2, aes_string(x = i_name, y = j_name, color = 'rnames', label = 'rnames')) +
  										  geom_text() + theme(legend.position = "none") +
  										  xlim(-1,1) + ylim(-1,1) + geom_vline(xintercept = 0) +
  										  geom_hline(yintercept = 0)
  		pnr <- pnr + 1
  	}
	}
	sshhr( do.call(arrangeGrob, c(plots, list(ncol = min(length(plots),2)))) ) %>%
	 	{if (shiny) . else print(.)}
}

#' Save factor scores to active dataset
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{full_factor}}
#'
#' @examples
#' \donttest{
#' result <- full_factor("diamonds",c("price","carat","table"))
#' save_factors(result)
#' head(diamonds)
#' }
#'
#' @export
save_factors <- function(object) {
  if (object$data_filter != "")
    return("Please deactivate data filters before trying to save factor scores")
  object$fres$scores %>% as.data.frame %>%
    changedata(object$dataset, vars = ., var_names = paste0("fac", 1:ncol(.)))
}

#' Sort and clean loadings
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/full_factor.html} for an example in Radiant
#'
#' @param floadings Data frame with loadings
#' @param fsort Sort factor loadings
#' @param cutoff Show only loadings with (absolute) values above cutoff (default = 0)
#' @param dec Number of decimals to show
#'
#' @examples
#' result <- full_factor("diamonds",c("price","carat","table","x","y"))
#' clean_loadings(result$floadings, TRUE, .5, 2)
#'
#' @export
clean_loadings <- function(floadings,
                           cutoff = 0,
                           fsort = FALSE,
                           dec = 8) {

	floadings %<>%
		{if (fsort) select(fa.sort(.), -order) else .}

	if (cutoff == 0) {
	  floadings %<>% round(dec)
  } else {
  	ind <- abs(floadings) < cutoff
  	floadings %<>% round(dec)
  	floadings[ind] <- ""
  }
  floadings
}
