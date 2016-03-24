#' Sample size calculation for comparisons
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sample_size_comp.html} for an example in Radiant
#'
#' @param type Choose "mean" or "proportion"
#' @param conf Confidence level
#' @param power Power
#' @param diff Detectible difference
#' @param prop Initial proportion
#' @param alt Two or one sided test
#'
#' @return A list of variables defined in sample_size_comp as an object of class sample_size_comp
#'
#' @seealso \code{\link{summary.sample_size_comp}} to summarize results
#' @export
sample_size_comp <- function(type,
                             n = NULL,
                             p1 = .5,
                             p2 = NULL,
                             delta = NULL,
                             sd = 1,
                             conf_lev = .95,
                             power = NULL,
                             alternative = "two.sided") {

	# Za <- ifelse(alt == "Two sided", qnorm(1 - (1-conf)/2), qnorm(conf))
	# Zb <- ifelse(alt == "Two sided", qnorm(1 - (1-power)/2), qnorm(power))
  if (!is.null(n) && is.na(n)) n <- NULL
  if (!is.null(power) && is.na(power)) n <- NULL

	if (type == 'mean') {
    if (!is.null(delta) && is.na(delta)) n <- NULL

    res <-
      power.t.test(n = n, delta = delta, sd = sd, sig.level = 1 - conf_lev, power = power, alternative = alternative) %>%
      tidy
	} else {
    if (!is.null(p2) && is.na(p2)) n <- NULL
		# n <- ((Za * sqrt(2 * prop * (1 - prop))) + (Zb * sqrt(prop * (1 - prop) + (prop + diff)*(1 - prop - diff))))^2 / diff^2
	  # ?power.prop.test
	  # ?power.t.test

    res <-
      power.prop.test(p1 = p1, p2 = p2, sig.level = 1 - conf_lev, power = power, alternative = alternative) %>%
      tidy
	}

	# n <- ceiling(n)

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
	cat("Sample size calculation for comparisons\n")

	if (object$type == "mean") {
	  cat("Calculation type     : Mean\n")
	} else {
	  cat("Type: Proportion\n")
	}

	# cat("Confidence level     :", object$zval, "\n")
	cat("Confidence level     :", object$conf, "\n")
	cat("Power                :", object$power, "\n")
	cat("Alternative          :", object$alternative, "\n")
	# cat("\nRequired sample size     :", nrprint(object$n, dec = 0))
  # rm(object)
	print(object$res, row.names = FALSE)
}

# library(radiant)

# sample_size_comp(type = "prop", conf = .95, power = .8, diff = .01, prop = .5, alt = "Two sided") %>% summary
# sample_size_comp(type = "prop", conf = .95, power = .8, diff = .01, prop = .5, alt = "One sided") %>% summary
# sample_size_comp(type = "prop", conf = .95, power = .95, diff = .005, prop = .03, alt = "One sided") %>% summary
# sample_size_comp(type = "prop", conf = .95, power = .8, diff = .15, prop = .3, alt = "Two sided") %>% summary
# power.prop.test()
# library(broom)
# power.prop.test(p1 = .3, p2 = 0.15, sig.level=.05, power=0.80) %>% tidy

# power.t.test(n = NULL, delta = 1, sd = 1, sig.level = 1 - .95, power = .8, alternative = "two.sided") %>% tidy
# power.t.test(n = NULL, delta = -1, sd = 1, sig.level = 1 - .95, power = .8, alternative = "two.sided") %>% tidy
# power.t.test(n = 100, delta = 1, sd = 1, sig.level = 1 - .95, power = NULL, alternative = "two.sided") %>% tidy

# sample_size_comp(type = "mean", delta = 1, sd = 1, conf_lev = .95, power = .8)

# power.prop.test(p1 = .3, p2 = .15, sig.level = 1 - .95, power = .8, alternative = "two.sided") %>% tidy
# power.prop.test(p1 = .3, p2 = .45, sig.level = 1 - .95, power = .8, alternative = "two.sided") %>% tidy

# sample_size_comp(type = "prop", p1 = .3, p2 = .15, conf_lev = .95, power = .8)

