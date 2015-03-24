#' Compare a sample proportion to a population proportion
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param sp_var The variable selected for the proportion comparison
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param sp_levels The factor level selected for the proportion comparison
#' @param sp_comp_value Population value to compare to the sample proportion
#' @param sp_alternative The alternative hypothesis ("two.sided", "greater", or "less")
#' @param sp_sig_level Span of the confidence interval
#'
#' @return A list of variables used in single_prop as an object of class single_prop
#'
#' @examples
#' result <- single_prop("diamonds","clarity", sp_levels = "IF", sp_comp_value = 0.05)
#'
#' @seealso \code{\link{summary.single_prop}} to summarize the results
#' @seealso \code{\link{plot.single_prop}} to plot the results
#'
#' @export
single_prop <- function(dataset, sp_var,
                        data_filter = "",
                        sp_levels = "",
                        sp_comp_value = 0.5,
                        sp_alternative = "two.sided",
                        sp_sig_level = .95) {

	dat <- getdata(dataset, sp_var, filt = data_filter) %>% mutate_each(funs(as.factor))

	levs <- levels(dat[,sp_var])
	if(sp_levels != "") {
		if(sp_levels %in% levs && levs[1] != sp_levels) {
			dat[,sp_var] %<>% as.character %>% as.factor %>% relevel(sp_levels)
			levs <- levels(dat[,sp_var])
		}
	}

	n <- nrow(dat)
	ns <- sum(dat == sp_levels)
	# use binom.test for exact
	prop.test(ns, n, p = sp_comp_value, alternative = sp_alternative,
	           conf.level = sp_sig_level, correct = FALSE) %>% tidy -> res

  environment() %>% as.list %>% set_class(c("single_prop",class(.)))
}

#' Summary method for the single_prop function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{single_prop}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_prop("diamonds","clarity", sp_levels = "IF", sp_comp_value = 0.05)
#' summary(result)
#'
#' @seealso \code{\link{single_prop}} to generate the results
#' @seealso \code{\link{plot.single_prop}} to plot the results
#'
#' @export
summary.single_prop <- function(object, ...) {

  cat("Single proportion test\n")
	cat("Data     :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
	cat("Variable :", object$sp_var, "\n")

	hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[object$sp_alternative]

	cat("Null hyp.: the proportion of", object$sp_levels, "in", object$sp_var, "=",
	    object$sp_comp_value, "\n")
	cat("Alt. hyp.: the proportion of", object$sp_levels, "in", object$sp_var, hyp_symbol,
	    object$sp_comp_value, "\n\n")

	# determine lower and upper % for ci
	{100 * (1-object$sp_sig_level)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%") -> ci_perc

	# object$res$n <- nrow(object$dat)
	res <- round(object$res, 3) 	# restrict to 3 decimal places
	res$ns <- object$ns
	res$n <- object$n
	names(res) <- c("prop","chisq.value","p.value","df", ci_perc[1], ci_perc[2],
									"ns","n")
	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

#' Plot method for the single_prop function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{single_prop}}
#' @param sp_plots Plots to generate. "hist" shows a histogram of the data along with vertical lines that indicate the sample proportion and the confidence interval. "simulate" shows the location of the sample proportion and the comparison value (sp_comp_value). Simulation is used to demonstrate the sampling variability in the data under the null-hypothesis
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_prop("diamonds","clarity", sp_levels = "IF", sp_comp_value = 0.05)
#' plot(result, sp_plots = c("hist", "simulate"))
#'
#' @seealso \code{\link{single_prop}} to generate the result
#' @seealso \code{\link{summary.single_prop}} to summarize the results
#'
#' @export
plot.single_prop <- function(x,
                             sp_plots = "hist",
                             ...) {

  object <- x; rm(x)

	lev_name <- object$levs[1]

 	plots <- list()
	if("hist" %in% sp_plots) {
		plots[[which("hist" == sp_plots)]] <-
			ggplot(object$dat, aes_string(x = object$sp_var, fill = object$sp_var)) +
	 			geom_histogram(alpha=.7) +
	 	 		ggtitle(paste0("Single proportion: ", lev_name, " in ", object$sp_var))
	}
	if("simulate" %in% sp_plots) {
		simdat <- rbinom(1000, prob = object$sp_comp_value, object$n) %>%
								divide_by(object$n) %>%
							  data.frame %>%
							  set_colnames(lev_name)

		ci_perc <- {if(object$sp_alternative == 'two.sided') {
									{(1-object$sp_sig_level)/2}  %>% c(., 1 - .)
								} else if(object$sp_alternative == 'less') {
									{1-object$sp_sig_level}
								} else {
									object$sp_sig_level
								}} %>%
									quantile(simdat[,lev_name], probs = . )

		bw <- simdat %>% range %>% diff %>% divide_by(20)

		plots[[which("simulate" == sp_plots)]] <-
			ggplot(simdat, aes_string(x=lev_name)) +
				geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
				geom_vline(xintercept = object$sp_comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = object$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = ci_perc,
				           color = 'red', linetype = 'longdash', size = .5) +
	 	 		ggtitle(paste0("Simulated proportions if null hyp. is true (", lev_name, " in ", object$sp_var, ")"))
	}

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}
