#' Sample size calculation for comparisons
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sample_size_comp.html} for an example in Radiant
#'
#' @param type Choose "mean" or "proportion"
#' @param n Sample size
#' @param p1 Proportion 1 (only used when "proportion" is selected)
#' @param p2 Proportion 2 (only used when "proportion" is selected)
#' @param delta Difference in means between two groups (only used when "mean" is selected)
#' @param sd Standard deviation (only used when "mean" is selected)
#' @param conf_lev Confidence level
#' @param power Power
#' @param ratio Sampling ratio (n1 / n2)
#' @param alternative Two or one sided test
#'
#' @return A list of variables defined in sample_size_comp as an object of class sample_size_comp
#'
#' @seealso \code{\link{summary.sample_size_comp}} to summarize results
#' @export
sample_size_comp <- function(type,
                             n = NULL,
                             p1 = NULL,
                             p2 = NULL,
                             delta = NULL,
                             sd = NULL,
                             conf_lev = NULL,
                             power = NULL,
                             ratio = 1,
                             alternative = "two.sided") {

  if (!is.null(n) && is.na(n)) n <- NULL
  if (!is.null(power) && is.na(power)) power <- NULL
  if (!is.null(conf_lev) && is.na(conf_lev)) conf_lev <- NULL
  sig.level <- if (is.null(conf_lev)) NULL else 1 - conf_lev
  adj <- ifelse (alternative == "two.sided", 2, 1)

	if (type == 'mean') {
    if (!is.null(delta) && is.na(delta)) delta <- NULL
    if (!is.null(delta)) delta <- abs(delta)
    if (!is.null(sd) && is.na(sd)) sd <- NULL

    nr_null <- is.null(n)+is.null(power)+is.null(delta)+is.null(sd)+is.null(conf_lev)
    if (nr_null == 0 || nr_null > 1)
    	return("Exactly one of 'Sample size', 'Delta', 'Std. deviation',\n'Confidence level', and 'Power' must be blank or NULL" %>% set_class(c("sample_size_comp",class(.))))

    res <-
      power.t.test(n = n, delta = delta, sd = sd, sig.level = sig.level, power = power, alternative = alternative) %>%
      tidy

	  ## adjustment based on http://powerandsamplesize.com/Calculators/Compare-2-Means/2-Sample-Equality
	  if (is.null(ratio) || is.na(ratio)) ratio <- 1
    n2 <-
      (1 + 1 / ratio) *
      (res$sd * (qnorm(1 - res$sig.level/adj) + qnorm(res$power)) /
      (res$delta))^2
    n2 <- ceiling(n2)
    n1 <- ceiling(ratio * n2)

	} else {
    if (!is.null(p1) && is.na(p1)) p1 <- NULL
    if (!is.null(p2) && is.na(p2)) p2 <- NULL

    if (!is.null(p1) && !is.null(p2)) {
    	if (p1 == p2)
    	  return("Proportion 1 and 2 should not be set equal. Please change the proportion values" %>% set_class(c("sample_size_comp",class(.))))
    }

    nr_null <- is.null(n)+is.null(power)+is.null(p1)+is.null(p2)+is.null(conf_lev)
    if (nr_null == 0 || nr_null > 1)
    	return("Exactly one of 'Sample size', 'Proportion 1', 'Proportion 2',\n'Confidence level', and 'Power' must be blank or NULL" %>% set_class(c("sample_size_comp",class(.))))

    res <-
      power.prop.test(n = n, p1 = p1, p2 = p2, sig.level = sig.level, power = power, alternative = alternative) %>%
      tidy

	  ## adjustment based on http://powerandsamplesize.com/Calculators/Compare-2-Proportions/2-Sample-Equality
	  if (is.null(ratio) || is.na(ratio)) ratio <- 1
	  adj <- ifelse (alternative == "two.sided", 2, 1)
    n2 <-
      (res$p1 * (1 - res$p1)/ratio + res$p2 * (1 - res$p2)) *
      ((qnorm(1 - res$sig.level/adj) + qnorm(res$power))/(res$p1 - res$p2))^2
    n2 <- ceiling(n2)
    n1 <- ceiling(ratio * n2)
	}

	res$n <- nrprint(ceiling(res$n), dec = 0)
	res$n1 <- nrprint(n1, dec = 0)
	res$n2 <- nrprint(n2, dec = 0)

  environment() %>% as.list %>% set_class(c("sample_size_comp",class(.)))
}

#' Summary method for the sample_size_comp function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sample_size_comp} for an example in Radiant
#'
#' @param object Return value from \code{\link{sample_size_comp}}
#' @param ... further arguments passed to or from other methods
#'
#'
#' @seealso \code{\link{sample_size_comp}} to generate the results
#'
#' @export
summary.sample_size_comp <- function(object, ...) {

  if (is.character(object)) return(object)

	cat("Sample size calculation for comparisons\n")

	cat("Type            :", object$type, "\n")
	if (object$ratio == 1) {
    cat("Sample size     :", object$res$n, "\n")
	} else {
	  cat(paste0("Sample size 1   : ", object$res$n1, " (", object$res$n2, " x ", object$ratio, ")\n"))
	  cat(paste0("Sample size 2   : ", object$res$n2, "\n"))
	}

	if (object$type == "mean") {
		cat("Delta           :", object$res$delta, "\n")
		cat("Std. deviation  :", object$res$sd, "\n")
	} else {
		cat("Proportion 1    :", object$res$p1, "\n")
		cat("Proportion 2    :", object$res$p2, "\n")
	}
	cat("Confidence level:", 1 - object$res$sig.level, "\n")
	cat("Power           :", object$res$power, "\n")
	cat("Ratio (n1 / n2) :", object$ratio, "\n")
	cat("Alternative     :", object$alternative, "\n\n")
}
