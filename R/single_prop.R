#' Compare a sample proportion to a population proportion
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var The variable selected for the proportion comparison
#' @param lev The factor level selected for the proportion comparison
#' @param comp_value Population value to compare to the sample proportion
#' @param alternative The alternative hypothesis ("two.sided", "greater", or "less")
#' @param conf_lev Span of the confidence interval
#' @param dec Number of decimals to show
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of variables used in single_prop as an object of class single_prop
#'
#' @examples
#' result <- single_prop("diamonds","cut")
#' result <- single_prop("diamonds","clarity", lev = "IF", comp_value = 0.05)
#'
#' @seealso \code{\link{summary.single_prop}} to summarize the results
#' @seealso \code{\link{plot.single_prop}} to plot the results
#'
#' @export
single_prop <- function(dataset, var,
                        lev = "",
                        comp_value = 0.5,
                        alternative = "two.sided",
                        conf_lev = .95,
                        dec = 3,
                        data_filter = "") {

	dat <- getdata(dataset, var, filt = data_filter, na.rm = FALSE) %>% mutate_each(funs(as.factor))
	if (!is_string(dataset)) dataset <- "-----"

  ## removing any missing values
	miss <- n_missing(dat)
  dat <- na.omit(dat)

	levs <- levels(dat[[var]])
	if (lev != "") {
		if (lev %in% levs && levs[1] != lev) {
			dat[[var]] %<>% as.character %>% as.factor %>% relevel(lev)
			levs <- levels(dat[[var]])
		}
	} else {
		lev <- levs[1]
	}

	n <- nrow(dat)
	ns <- sum(dat == lev)
	p <- ns / n

  dat_summary <- data.frame(
    diff = p - comp_value,
    prop = p,
    mean = n*p,
		sd = sqrt(n*p*(1-p)),
		n = n,
	  n_missing = miss
  )

	## use binom.test for exact
	res <- binom.test(ns, n, p = comp_value, alternative = alternative,
	                  conf.level = conf_lev) %>% tidy

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
#' result <- single_prop("diamonds","clarity", lev = "IF", comp_value = 0.05)
#' summary(result)
#' diamonds %>% single_prop("clarity", lev = "IF", comp_value = 0.05) %>% summary
#'
#' @seealso \code{\link{single_prop}} to generate the results
#' @seealso \code{\link{plot.single_prop}} to plot the results
#'
#' @export
summary.single_prop <- function(object, ...) {

	dec <- object$dec

  cat("Single proportion test (binomial exact)\n")
	cat("Data      :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter    :", gsub("\\n","", object$data_filter), "\n")
	cat("Variable  :", object$var, "\n")
	cat("Level     :", object$lev, "in", object$var, "\n")
	cat("Confidence:", object$conf_lev, "\n")

	hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[object$alternative]

	cat("Null hyp. : the proportion of", object$lev, "in", object$var, "=",
	    object$comp_value, "\n")
	cat("Alt. hyp. : the proportion of", object$lev, "in", object$var, hyp_symbol,
	    object$comp_value, "\n\n")

	## determine lower and upper % for ci
	ci_perc <- ci_label(object$alternative, object$conf_lev)

	## print summary statistics
  print(object$dat_summary[-1] %>% round(dec) %>% as.data.frame, row.names = FALSE)
	cat("\n")

	res <- object$res
	res <- bind_cols(
	         data.frame(diff = object$dat_summary[["diff"]]),
	         res[,-1]
	       ) %>%
	       select(-matches("parameter")) %>%
	       as.data.frame

	names(res) <- c("diff","ns","p.value", ci_perc[1], ci_perc[2])
	res %<>% round(dec) 	# restrict the number of decimals
	res$` ` <- sig_stars(res$p.value)
	if (res$p.value < .001) res$p.value <- "< .001"

	## print statistics
	print(res, row.names = FALSE)
	cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

}

#' Plot method for the single_prop function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{single_prop}}
#' @param plots Plots to generate. "bar" shows a bar chart of the data. The "simulate" chart shows the location of the sample proportion and the comparison value (comp_value). Simulation is used to demonstrate the sampling variability in the data under the null-hypothesis
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_prop("diamonds","clarity", lev = "IF", comp_value = 0.05)
#' plot(result, plots = c("hist", "simulate"))
#' result <- single_prop("titanic","pclass", lev = "1st")
#' plot(result, plots = c("hist","simulate"))
#'
#' @seealso \code{\link{single_prop}} to generate the result
#' @seealso \code{\link{summary.single_prop}} to summarize the results
#'
#' @export
plot.single_prop <- function(x,
                             plots = "bar",
                             shiny = FALSE,
                             ...) {

	## bar used to called hist - changed for consistency with compare_props
	plots %<>% gsub("hist","bar",.)

  object <- x; rm(x)

	lev_name <- object$levs[1]

 	plot_list <- list()
	if ("bar" %in% plots) {
		plot_list[[which("bar" == plots)]] <-
			# ggplot(object$dat, aes_string(x = object$var, fill = object$var)) +
	 	# 		geom_histogram(alpha = .7) +
		 # 		scale_y_continuous(labels = percent) +
	 	#  		ggtitle(paste0("Single proportion: ", lev_name, " in ", object$var)) +
	 	#  		theme(legend.position = "none")


			ggplot(object$dat, aes_string(x = object$var, fill = object$var)) +
	 	 		geom_bar(aes(y = (..count..)/sum(..count..)), alpha = .7) +
		 		scale_y_continuous(labels = scales::percent) +
	 	 		ggtitle(paste0("Single proportion: ", lev_name, " in ", object$var)) +
	 	 		ylab("") + theme(legend.position = "none")

	}
	if ("simulate" %in% plots) {
		simdat <- rbinom(1000, prob = object$comp_value, object$n) %>%
								divide_by(object$n) %>%
							  data.frame %>%
							  set_colnames(lev_name)

    cip <- ci_perc(simdat[[lev_name]], object$alternative, object$conf_lev) %>% set_names(NULL)

		bw <- simdat %>% range %>% diff %>% divide_by(20)

		# to avoid problems with levels that start with numbers or contain spaces
		# http://stackoverflow.com/questions/13445435/ggplot2-aes-string-fails-to-handle-names-starting-with-numbers-or-containing-s
		names(simdat) <- "col1"

		plot_list[[which("simulate" == plots)]] <-
			ggplot(simdat, aes(x = col1)) +
				geom_histogram(fill = 'blue', binwidth = bw, alpha = .3) +
				geom_vline(xintercept = object$comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = object$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = cip, color = 'red', linetype = 'longdash', size = .5) +
	 	 		ggtitle(paste0("Simulated proportions if null hyp. is true (", lev_name, " in ", object$var, ")")) +
	 	 		labs(x = paste0("Level ",lev_name, " in variable ", object$var))
	}

	sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
	  { if (shiny) . else print(.) }
}
