#' Evaluate if data are appropriate for PCA / Factor analysis
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/pre_factor.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param pf_var Variables to include in the analysis
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in the function as an object of class pre_factor
#'
#' @examples
#' result <- pre_factor("diamonds",c("price","carat","table"))
#'
#' @seealso \code{\link{summary.pre_factor}} to summarize results
#' @seealso \code{\link{plot.pre_factor}} to plot results
#'
#' @importFrom psych KMO cortest.bartlett
#'
#' @export
pre_factor <- function(dataset, pf_var,
                       data_filter = "") {

	dat <- getdata(dataset, pf_var, filt = data_filter)
	nrObs <- nrow(dat)

	if(nrObs <= ncol(dat)) {
		ret <- "Data should have more observations than variables.\nPlease reduce the number of variables." %>%
						 set_class(c("pre_factor",class(.)))
		return(ret)
	}

  cmat <- cor(dat)
	btest <- cortest.bartlett(cmat, nrow(dat))
	pre_kmo <- KMO(cmat)
	pre_eigen <- eigen(cmat)$values

  err_mess <- "The selected variables are perfectly collinear. Please check the correlations\nand remove any variable with a correlation of 1 or -1 from the analysis"
  if(det(cmat) > 0) {
    scmat <- try(solve(cmat), silent = TRUE)
    if(is(scmat, 'try-error')) {
    	pre_r2 <- err_mess
    } else {
    	pre_r2 <- {1 - (1 / diag(scmat))} %>%
    							data.frame %>%
    							set_colnames('Rsq')
    }
  } else {
  	pre_r2 <- err_mess
  }

  rm(dat)

  environment() %>% as.list %>% set_class(c("pre_factor",class(.)))
}

#' Summarize method for the pre_factor function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/pre_factor.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{pre_factor}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- pre_factor("diamonds",c("price","carat","table"))
#' summary(result)
#' result <- pre_factor("computer","HighEnd:Business")
#' summary(result)
#'
#' @seealso \code{\link{pre_factor}} to calculate results
#' @seealso \code{\link{plot.pre_factor}} to plot results
#'
#' @export
summary.pre_factor <- function(object, ...) {

	if(is.character(object)) return(cat(object))

	if(object$pre_r2 %>% is.character) {
		cat(object$pre_r2)
		return(invisible())
	}

	cat("Pre-factor analysis diagnostics\n")
	cat("Data        :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables   :", paste0(object$pf_var, collapse=", "), "\n")
	cat("Observations:", object$nrObs, "\n")

	btest <- object$btest
	cat("\nBartlett test\n")
	cat("Null hyp.: variables are not correlated\n")
	cat("Alt. hyp.: variables are correlated\n")
	bt <- object$btest$p.value %>% { if(. < .001) "< .001" else round(.,3) }
	cat(paste0("Chi-square: ", round(object$btest$chisq,2), " df(",
	    object$btest$df, "), p.value ", bt, "\n"))

	cat("\nKMO test: ", round(object$pre_kmo$MSA,2), "\n")
	# cat("\nMeasures of sampling adequacy:\n")
	# print(object$pre_kmo$MSAi, digits = 3)

  cat("\nVariable collinearity:\n")
  print(round(object$pre_r2, 2), digits = 2)

	cat("\n")
	object$pre_eigen %>%
	  { data.frame(Factor = 1:length(.),
							   Eigenvalues = round(.,2),
							   `Variance %` = ./sum(.),
							   `Cumulative %` = cumsum(./sum(.)),
							   check.names = FALSE) } %>%
	  round(2) %>%
	  print(., row.names = FALSE)
}

#' Plot method for the pre_factor function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/pre_factor.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{pre_factor}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- pre_factor("diamonds",c("price","carat","table"))
#' plot(result)
#'
#' @seealso \code{\link{pre_factor}} to calculate results
#' @seealso \code{\link{summary.pre_factor}} to summarize results
#'
#' @export
plot.pre_factor <- function(x, ...){

	object <- x; rm(x)

	if(is.character(object)) return(invisible())

	if(object$pre_r2 %>% is.character) return(invisible())

	data.frame(y = object$pre_eigen, x = 1:length(object$pre_eigen)) %>%
	ggplot(aes(x=x, y=y, group = 1)) +
    geom_line(colour="blue", linetype = 'dotdash', size=.7) +
    geom_point(colour="blue", size=4, shape=21, fill="white") +
		geom_hline(yintercept = 1, color = 'black', linetype = 'solid', size = 1) +
	  labs(list(title = "Screeplot of Eigenvalues", x = "# factors", y = "Eigenvalues"))
}
