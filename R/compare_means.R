#' Compare means for two or more variables
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param cm_var1 A numeric variable or factor selected for comparison
#' @param cm_var2 One or more numeric variables for comparison. If cm_var1 is a factor only one variable can be selected and the mean of this variable is compared across (factor) levels of cm_var1
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param cm_paired Are samples indepent ("independent") or not ("paired")
#' @param cm_alternative The alternative hypothesis ("two.sided", "greater" or "less")
#' @param cm_sig_level Span of the confidence interval
#' @param cm_adjust Adjustment for multiple comparisons ("none" or "bonf" for Bonferroni)
#'
#' @return A list of all variables defined in the function as an object of class compare_means
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#'
#' @seealso \code{\link{summary.compare_means}} to summarize results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
compare_means <- function(dataset, cm_var1, cm_var2,
                          data_filter = "",
                          cm_paired = "independent",
                          cm_alternative = "two.sided",
                          cm_sig_level = .95,
                          cm_adjust = "none") {

	vars <- c(cm_var1, cm_var2)
	dat <- getdata(dataset, vars, filt = data_filter)

	if(dat[,cm_var1] %>% is.factor) {
		colnames(dat) <- c("variable","values")
	} else {
		dat %<>% gather_("variable", "values", vars)
	}

	# check variances in the data
  if(dat %>% summarise_each(., funs(var(.,na.rm = TRUE))) %>% min %>% {. == 0})
  	return("Test could not be calculated. Please select another variable.")

	# resetting option to independent if the number of observations is unequal
  if(cm_paired == "paired")
    if(summary(dat$variable) %>% { max(.) != min(.) })
      cm_paired <- "independent (obs. per level unequal)"

	##############################################
	# flip the order of pairwise testing - part 1
	##############################################
  flip_alt <- c("two.sided" = "two.sided",
                "less" = "greater",
                "greater" = "less")
	##############################################

	pairwise.t.test(dat[,"values"], dat[,"variable"], pool.sd = FALSE,
	                p.adjust.method = cm_adjust, paired = cm_paired == "paired",
                  alternative = flip_alt[cm_alternative]) %>% tidy -> res

	##############################################
	# flip the order of pairwise testing - part 2
	##############################################
	res[,c("group1","group2")] <- res[,c("group2","group1")]
	##############################################

	# from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
	ci_calc <- function(se, n, conf.lev = .95)
	 	se * qt(conf.lev/2 + .5, n-1)

	dat %>%
		group_by_("variable") %>%
    summarise_each(funs(mean, n = length(.), sd,
                   			se = sd/sqrt(n),
                   			ci = ci_calc(se,n,cm_sig_level))) %>%
    rename_(.dots = setNames("variable", " ")) -> dat_summary

	vars <- paste0(vars, collapse=", ")
  environment() %>% as.list %>% set_class(c("compare_means",class(.)))
}

#' Summary method for the compare_means function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{compare_means}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#' summary(result)
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
summary.compare_means <- function(object, ...) {

	# result <- object
  if(object$cm_adjust == "bonf") {
    cat("Pairwise comparisons (bonferroni adjustment)\n")
  } else {
	  cat("Pairwise comparisons (no adjustment)\n")
  }

	cat("Data     :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables:", object$vars, "\n")
	cat("Samples  :", object$cm_paired, "\n\n")

  object$dat_summary[,-1] %<>% round(3)
  print(object$dat_summary %>% as.data.frame, row.names = FALSE)
	cat("\n")

  hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[object$cm_alternative]

  means <- object$dat_summary$mean
  names(means) <- object$dat_summary$` ` %>% as.character

	mod <- object$res
	mod$`Alt. hyp.` <- paste(mod$group1,hyp_symbol,mod$group2," ")
	mod$`Null hyp.` <- paste(mod$group1,"=",mod$group2, " ")
	mod$diff <- { means[mod$group1 %>% as.character] - means[mod$group2 %>% as.character] } %>% round(3)
	mod <- mod[,c("Alt. hyp.", "Null hyp.", "diff", "p.value")]
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
#' @param cm_plots One or more plots ("bar", "box", or "density")
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#' plot(result, cm_plots = c("bar","density"))
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{summary.compare_means}} to summarize results
#'
#' @export
plot.compare_means <- function(x,
                               cm_plots = "bar",
                               ...) {

	object <- x; rm(x)

	dat <- object$dat
	var1 <- colnames(dat)[1]
	var2 <- colnames(dat)[-1]

	# from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
	plots <- list()
	if("bar" %in% cm_plots) {
		colnames(object$dat_summary)[1] <- "variable"
		# use of `which` allows the user to change the order of the plots shown
		plots[[which("bar" == cm_plots)]] <- ggplot(object$dat_summary,
	    aes_string(x="variable", y="mean", fill="variable")) +
	    geom_bar(stat="identity")  +
	 		geom_errorbar(width=.1, aes(ymin=mean-ci, ymax=mean+ci)) +
	 		geom_errorbar(width=.05, aes(ymin=mean-se, ymax=mean+se), colour = "blue")
	}

	# graphs on full data
	if("box" %in% cm_plots) {
		plots[[which("box" == cm_plots)]] <-
			ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) +
				geom_boxplot(alpha=.7)
	}

	if("density" %in% cm_plots) {
		plots[[which("density" == cm_plots)]] <-
			ggplot(dat, aes_string(x=var2, fill=var1)) + geom_density(alpha=.7)
	}

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}
