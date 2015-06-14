###############################
# Sample size
###############################
#' Sample size calculation
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sample_size.html} for an example in Radiant
#'
#' @param type Choose "mean" or "proportion"
#' @param err_mean Acceptable Error for Mean
#' @param sd_mean Standard deviation for Mean
#' @param err_prop Acceptable Error for Proportion
#' @param p_prop Initial proportion estimate for Proportion
#' @param zval Z-value
#' @param incidence Incidence rate (i.e., fraction of valid respondents)
#' @param response Response rate
#' @param pop_correction Apply correction for population size ("yes","no")
#' @param pop_size Population size
#'
#' @return A list of variables defined in sample_size as an object of class sample_size
#'
#' @examples
#' result <- sample_size(type = "mean", err_mean = 2, sd_mean = 10)
#'
#' @seealso \code{\link{summary.sample_size}} to summarize results
#' @export
sample_size <- function(type = "mean",
                        err_mean = 2,
                        sd_mean = 10,
                        err_prop = .1,
                        p_prop = .5,
                        zval = 1.96,
                        incidence = 1,
                        response = 1,
                        pop_correction = "no",
                        pop_size = 1000000) {

	if (type == 'mean') {
		if (is.na(err_mean)) return("Please select an error value greater 0.")

		n <- (zval^2 * sd_mean^2) / err_mean^2
	} else {

		if (is.na(err_prop)) return("Please select an error value greater 0.")
		n <- (zval^2 * p_prop * (1 - p_prop)) / err_prop^2
	}

	if (pop_correction == 'yes')
		n <- n * pop_size / ((n - 1) + pop_size)

	n <- ceiling(n)

  environment() %>% as.list %>% set_class(c("sample_size",class(.)))
}

#' Summary method for the sample_size function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sample_size} for an example in Radiant
#'
#' @param object Return value from \code{\link{sample_size}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- sample_size(type = "mean", err_mean = 2, sd_mean = 10)
#' summary(result)
#'
#' @seealso \code{\link{sample_size}} to generate the results
#'
#' @export
summary.sample_size <- function(object, ...) {
	cat("Sample size calculation\n")

	if (object$type == "mean") {
	  cat("Calculation type     : Mean\n")
		cat("Acceptable Error     :", object$err_mean, "\n")
		cat("Sample std. deviation:", object$sd_mean, "\n")
	} else {
	  cat("Type: Proportion\n")
		cat("Acceptable Error     :", object$err_prop, "\n")
		cat("Sample proportion    :", object$p_prop, "\n")
	}

	cat("Confidence level     :", object$zval, "\n")
	cat("Incidence rate       :", object$incidence, "\n")
	cat("Response rate        :", object$response, "\n")

	if (object$pop_correction == "no") {
		cat("Population correction: None\n")
	} else {
		cat("Population correction: Yes\n")
		cat("Population size      :", format(object$pop_size, big.mark = ",",
		    																 scientific = FALSE), "\n")
	}

	cat("\nRequired sample size     :", format(object$n, big.mark = ",",
	    																			 scientific = FALSE))
	cat("\nRequired contact attempts:", format(ceiling(object$n / object$incidence / object$response),
	    																			 big.mark = ",", scientific = FALSE))
	cat("\n\nChoose a Z-value:\n")

  for (z in c(.80, .85, .90, .95, .99))
    cat(paste0(100*z,"%\t"),-qnorm((1-z)/2) %>% round(2),"\n")

  rm(object)
}
