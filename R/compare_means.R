#' Compare means for two or more variables
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var1 A numeric variable or factor selected for comparison
#' @param var2 One or more numeric variables for comparison. If var1 is a factor only one variable can be selected and the mean of this variable is compared across (factor) levels of va1r
#' @param samples Are samples indepent ("independent") or not ("paired")
#' @param alternative The alternative hypothesis ("two.sided", "greater" or "less")
#' @param conf_lev Span of the confidence interval
#' @param comb Combinations to evaluate
#' @param adjust Adjustment for multiple comparisons ("none" or "bonf" for Bonferroni)
#' @param test T-test ("t") or Wilcox ("wilcox")
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables defined in the function as an object of class compare_means
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#' result <- diamonds %>% compare_means("cut","price")
#'
#' @seealso \code{\link{summary.compare_means}} to summarize results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
compare_means <- function(dataset, var1, var2,
                          samples = "independent",
                          alternative = "two.sided",
                          conf_lev = .95,
                          comb = "",
                          adjust = "none",
                          test = "t",
                          data_filter = "") {

	vars <- c(var1, var2)
	dat <- getdata(dataset, vars, filt = data_filter)
	if (!is_string(dataset)) dataset <- "-----"

	## in case : was used for var2
	vars <- colnames(dat)

	if (dat[[var1]] %>% is.factor) {
		colnames(dat) <- c("variable","values")
		cname <- var1
	} else {
		dat %<>% gather_("variable", "values", vars)
		cname <- " "
  }

	## check there is variation in the data
  if (any(summarise_each(dat, funs(does_vary)) == FALSE))
  	return("Test could not be calculated (no variation). Please select another variable." %>%
  	       set_class(c("compare_means",class(.))))

	## resetting option to independent if the number of observations is unequal
  ## summary on factor gives counts
  if (samples == "paired") {
    if (summary(dat[["variable"]]) %>% {max(.) != min(.)})
      samples <- "independent (obs. per level unequal)"
  }

	levs <- levels(dat[["variable"]])
  cmb <- combn(levs, 2) %>% t %>% as.data.frame
  rownames(cmb) <- cmb %>% apply(1, paste, collapse = ":")
  colnames(cmb) <- c("group1","group2")

	if (!is_empty(comb)) {
		if (all(comb %in% rownames(cmb))) {
			cmb <- cmb[comb, ]
		} else {
			cmb <- cmb[1,]
  	}
	}

  res <- cmb
  res[ ,c("t.value","p.value", "df", "ci_low", "ci_high", "cis_low", "cis_high")] <- 0

  for (i in 1:nrow(cmb)) {
  	sel <- cmb[i,]
  	x <- filter_(dat, paste0("variable == '", sel[[1]], "'")) %>% .[["values"]]
  	y <- filter_(dat, paste0("variable == '", sel[[2]], "'")) %>% .[["values"]]

  	res[i,c("t.value","p.value", "df", "ci_low", "ci_high")] <-
  	  t.test(x, y, paired = samples == "paired", alternative = alternative, conf.level = conf_lev) %>%
  	  tidy %>% .[1, c("statistic", "p.value","parameter", "conf.low", "conf.high")]

  	## bootstrap confidence intervals
  	## seem almost identical, even with highly skewed data
		# nr_x <- length(x)
		# nr_y <- length(y)

  #   sim_ci <-
  #     replicate(1000,
  #               mean(sample(x, nr_x, replace = TRUE)) -
  #               mean(sample(y, nr_y, replace = TRUE))) %>%
		# 						quantile(probs = {(1-conf_lev)/2} %>% c(., 1 - .))

		# res[i, c("cis_low", "cis_high")] <- sim_ci

  }
  rm(x,y,sel)

	if (adjust != "none")
		res$p.value %<>% p.adjust(method = adjust)

	## from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
	ci_calc <- function(se, n, conf.lev = .95)
	 	se * qt(conf.lev/2 + .5, n - 1)

	dat_summary <-
	  dat %>%
		group_by_("variable") %>%
    summarise_each(funs(mean, n = length(.), sd,
                   			se = sd/sqrt(n),
                   			ci = ci_calc(se,n,conf_lev))) %>%
    rename_(.dots = setNames("variable", cname))

	vars <- paste0(vars, collapse = ", ")
  environment() %>% as.list %>% set_class(c("compare_means",class(.)))
}

#' Summary method for the compare_means function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{compare_means}}
#' @param show Show additional output (i.e., t.value, df, and confidence interval)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#' summary(result)
#' result <- diamonds %>% tbl_df %>% compare_means("x","y")
#' summary(result)
#' result <- diamonds %>% tbl_df %>% group_by(cut) %>% compare_means("x",c("x","y"))
#' summary(result)
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
summary.compare_means <- function(object, show = FALSE, ...) {

	if (is.character(object)) return(object)

  cat(paste0("Pairwise mean comparisons (", object$test, "-test)\n"))
	cat("Data      :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter    :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables :", object$vars, "\n")
	cat("Samples   :", object$samples, "\n")
	cat("Confidence:", object$conf_lev, "\n")
	cat("Adjustment:", if (object$adjust == "bonf") "Bonferroni" else "None", "\n\n")

  object$dat_summary[,-1] %<>% round(3)
  print(object$dat_summary %>% as.data.frame, row.names = FALSE)
	cat("\n")

  hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[object$alternative]

  means <- object$dat_summary$mean
  names(means) <- object$dat_summary[[1]] %>% as.character

	# determine lower and upper % for ci
	ci_perc <-
	  {100 * (1-object$conf_lev)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%")

	mod <- object$res
	mod$`Alt. hyp.` <- paste(mod$group1,hyp_symbol,mod$group2," ")
	mod$`Null hyp.` <- paste(mod$group1,"=",mod$group2, " ")
	mod$diff <- { means[mod$group1 %>% as.character] - means[mod$group2 %>% as.character] } %>% round(3)
	# mod[,"t.value"] %<>% round(3)

	if (show) {
	  mod <- mod[,c("Alt. hyp.", "Null hyp.", "diff", "t.value", "df", "ci_low", "ci_high", "p.value")]
	  # mod <- mod[,c("Alt. hyp.", "Null hyp.", "diff", "t.value", "df", "ci_low", "ci_high", "cis_low", "cis_high", "p.value")]
		if (!is.integer(mod[["df"]])) mod[["df"]] %<>% round(3)
		mod[,c("t.value", "ci_low","ci_high")] %<>% round(3)
	  mod <- rename_(mod, .dots = setNames(c("ci_low","ci_high"), ci_perc))
	} else {
	  mod <- mod[,c("Alt. hyp.", "Null hyp.", "diff", "p.value")]
	}

	mod$` ` <- sig_stars(mod$p.value)
	mod$p.value <- round(mod$p.value,3)
	mod$p.value[ mod$p.value < .001 ] <- "< .001"
	print(mod, row.names = FALSE, right = FALSE)
	cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' Plot method for the compare_means function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{compare_means}}
#' @param plots One or more plots ("bar", "density", "box", or "scatter")
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#' plot(result, plots = c("bar","density"))
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{summary.compare_means}} to summarize results
#'
#' @export
plot.compare_means <- function(x,
                               plots = "scatter",
                               shiny = FALSE,
                               ...) {

	if (is.character(x)) return(x)
	object <- x; rm(x)

	dat <- object$dat
	v1 <- colnames(dat)[1]
	v2 <- colnames(dat)[-1]
	var1 <- object$var1
	var2 <- object$var2

	## from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
	plot_list <- list()
	if ("bar" %in% plots) {
		colnames(object$dat_summary)[1] <- "variable"
		## use of `which` allows the user to change the order of the plots shown
		plot_list[[which("bar" == plots)]] <-
		  ggplot(object$dat_summary,
		    aes_string(x = "variable", y = "mean", fill = "variable")) +
		    geom_bar(stat = "identity")  +
		 		geom_errorbar(width = .1, aes(ymin = mean - ci, ymax = mean + ci)) +
		 		geom_errorbar(width = .05, aes(ymin = mean - se, ymax = mean + se), colour = "blue") +
		 		theme(legend.position = "none") +
		 		xlab(var1) + ylab(var2)
	}

	## graphs on full data
	if ("box" %in% plots) {
		plot_list[[which("box" == plots)]] <-
			ggplot(dat, aes_string(x = v1, y = v2, fill = v1)) +
				geom_boxplot(alpha = .7) + theme(legend.position = "none") +
	 		  xlab(var1) + ylab(var2)
	}

	if ("density" %in% plots) {
		plot_list[[which("density" == plots)]] <-
			ggplot(dat, aes_string(x = v2, fill = v1)) + geom_density(alpha = .7) +
	 		  xlab(var1) + ylab(var2) +
	 		  guides(fill = guide_legend(title = var1))

	}

	if ("scatter" %in% plots) {

    ymax <- max(dat[[v2]]) %>% {if (. < 0) 0 else .}
    ymin <- min(dat[[v2]]) %>% {if (. > 0) 0 else .}
		plot_list[[which("scatter" == plots)]] <-
      ggplot(dat, aes_string(x=v1, y=v2)) +
        geom_jitter(alpha = .3, position = position_jitter(width = 0.4, height = 0.1)) +
        geom_errorbar(stat = "hline", yintercept = "mean", width = .8, size = 1, color = "blue", aes(ymax = ..y.., ymin = ..y..)) +
        ylim(ymin,ymax) +
	 		  xlab(var1) + ylab(var2)
  }

	sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
 	  { if (shiny) . else print(.) }

}
