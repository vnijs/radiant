###############################
# Sample size
###############################
#' Sample size calculation
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sample_size.html} for an example in Radiant
#'
#' @param ss_type Choose "mean" or "proportion"
#' @param ss_mean_err Acceptable Error for Mean
#' @param ss_mean_s Standard deviation for Mean
#' @param ss_prop_err Acceptable Error for Proportion
#' @param ss_prop_p Initial proportion estimate for Proportion
#' @param ss_z Z-value
#' @param ss_incidence Incidence rate (i.e., fraction of valid respondents)
#' @param ss_response Response rate
#' @param ss_pop_correction Apply correction for population size ("yes","no")
#' @param ss_pop_size Population size
#'
#' @return A list of variables defined in sample_size as an object of class sample_size
#'
#' @examples
#' result <- sample_size(ss_type = "mean", ss_mean_err = 2, ss_mean_s = 10)
#'
#' @seealso \code{\link{summary.sample_size}} to summarize results
#' @export
sample_size <- function(ss_type = "mean",
                        ss_mean_err = 2,
                        ss_mean_s = 10,
                        ss_prop_err = .1,
                        ss_prop_p = .5,
                        ss_z = 1.96,
                        ss_incidence = 1,
                        ss_response = 1,
                        ss_pop_correction = "no",
                        ss_pop_size = 1000000) {

	if(ss_type == 'mean') {
		if(is.na(ss_mean_err)) return("Please select an error value greater 0.")

		n <- (ss_z^2 * ss_mean_s^2) / ss_mean_err^2
	} else {

		if(is.na(ss_prop_err)) return("Please select an error value greater 0.")
		n <- (ss_z^2 * ss_prop_p * (1 - ss_prop_p)) / ss_prop_err^2
	}

	if(ss_pop_correction == 'yes')
		n <- n * ss_pop_size / ((n - 1) + ss_pop_size)

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
#' result <- sample_size(ss_type = "mean", ss_mean_err = 2, ss_mean_s = 10)
#' summary(result)
#'
#' @seealso \code{\link{sample_size}} to generate the results
#'
#' @export
summary.sample_size <- function(object, ...) {
	cat("Sample size calculation\n")

	if(object$ss_type == "mean") {
	  cat("Calculation type     : Mean\n")
		cat("Acceptable Error     :", object$ss_mean_err, "\n")
		cat("Sample std. deviation:", object$ss_mean_s, "\n")
	} else {
	  cat("Type: Proportion\n")
		cat("Acceptable Error     :", object$ss_prop_err, "\n")
		cat("Sample proportion    :", object$ss_prop_p, "\n")
	}

	cat("Confidence level     :", object$ss_z, "\n")
	cat("Incidence rate       :", object$ss_incidence, "\n")
	cat("Response rate        :", object$ss_response, "\n")

	if(object$ss_pop_correction == "no") {
		cat("Population correction: None\n")
	} else {
		cat("Population correction: Yes\n")
		cat("Population size      :", format(object$ss_pop_size, big.mark = ",",
		    																 scientific = FALSE), "\n")
	}

	cat("\nRequired sample size     :", format(object$n, big.mark = ",",
	    																			 scientific = FALSE))
	cat("\nRequired contact attempts:", format(ceiling(object$n / object$ss_incidence / object$ss_response),
	    																			 big.mark = ",", scientific = FALSE))
	cat("\n\nChoose a Z-value:\n")

  for(z in c(.80, .85, .90, .95, .99))
    cat(paste0(100*z,"%\t"),-qnorm((1-z)/2) %>% round(2),"\n")

  rm(object)
}
