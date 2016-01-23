#' Evaluate if sample data for a categorical variable is consistent wtih a hypothesized distribution
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/goodness.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var A categorical variable
#' @param p Hypothesized distribution (either a numeric or character vector). If unspecified, defaults to an even distribution
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in cross_tabs as an object of class cross_tabs
#'
#' @examples
#' result <- goodness("newspaper", "Income")
#'
#' @seealso \code{\link{summary.goodness}} to summarize results
#' @seealso \code{\link{plot.goodness}} to plot results
#'
#' @export
goodness <- function(dataset, var, p = c(),
                     data_filter = "") {

	dat <- getdata(dataset, var, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  ## Use simulated p-values when
  # http://stats.stackexchange.com/questions/100976/n-1-pearsons-chi-square-in-r
  # http://stats.stackexchange.com/questions/14226/given-the-power-of-computers-these-days-is-there-ever-a-reason-to-do-a-chi-squa/14230#14230
  # http://stats.stackexchange.com/questions/62445/rules-to-apply-monte-carlo-simulation-of-p-values-for-chi-squared-test

  # library(radiant)
  # dat <- diamonds
  # var <- "cut"
  # p <- 0

  ## creating and cleaning up the table
	tab <- table(dat[[var]])
	tab[is.na(tab)] <- 0
	tab %<>% as.table

	if (length(p) == 0) {
		p <- rep(1/length(tab), length(tab))
	} else if (is.character(p)) {

	  # p <- ".30 .70"
		# p <- ".2 .3 .3 .15 .05"
		# p <- "1/6    \n2/12 1/6 1/4 1/4"
		# p <- "1/6 1/12 1/6 abbb"

		p <- unlist(strsplit(p, "\\s+"))
		cp <- c()
		for (i in p) {
			res <- try(eval(parse(text = i)), silent = TRUE)
		  if (is(res, 'try-error')) {
		    mess <- paste0("Invalid inputs:\n\n", attr(res,"condition")$message)
		    return(mess %>% set_class(c("goodness", class(.))))
		  }
		  cp <- c(cp, res)
		}
		p <- cp
	  if (sum(p) != 1) {
		  mess <- c("error",paste0("Probabilities do not sum to 1 (",round(sum(p),3),")\nUse fractions if appropriate."))
		  return(mess %>% set_class(c("goodness", class(.))))
		}
	}

	cst <- sshhr( chisq.test(tab, p = p, correct = FALSE) )

	## adding the % deviation table
	# cst$deviation <- with(cst, (observed-expected) / expected)
	cst$chi_sq	<- with(cst, (observed - expected)^2 / expected)

	## dat not needed in summary or plot
	rm(dat)

  environment() %>% as.list %>% set_class(c("goodness",class(.)))
}

#' Summary method for the goodness function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/goodness} for an example in Radiant
#'
#' @param object Return value from \code{\link{goodness}}
#' @param check Show table(s) for the selected variable (var). "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "dev_perc" for the percentage difference between the observed and expected frequencies (i.e., (o - e) / e)
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' result <- goodness("newspaper", "Income", c(.3, .7))
#' summary(result, check = c("observed","expected","chi_sq"))
#' newspaper %>% goodness("Income", c(.3, .7)) %>% summary("observed")
#'
#' @seealso \code{\link{goodness}} to calculate results
#' @seealso \code{\link{plot.goodness}} to plot results
#'
#' @export
summary.goodness <- function(object,
                             check = "",
                             ...) {

  # object <- result

  cat("Goodness of fit test\n")
	cat("Data     :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
	cat("Variable :", object$var, "\n")
	cat("Specified:", object$p, "\n")
	cat("Null hyp.: the distribution of", object$var, "is consistent with the specified distribution\n")
	cat("Alt. hyp.: the distribution of", object$var, "is not consistent with the specified distribution\n")

	# object$cst$observed %>% rownames %>% c(., "Total") -> rnames

	if ("observed" %in% check) {
		cat("\nObserved:\n")
		object$cst$observed %>%
			{.["Total"] <- sum(.); .} %>%
			print
	}

	if ("expected" %in% check) {
		cat("\nExpected: total x p\n")
		object$cst$expected %>%
			{.["Total"] <- sum(.); .} %>%
			round(2) %>%
			print
	}

	if ("chi_sq" %in% check) {
		cat("\nContribution to chi-squared: (o - e)^2 / e\n")
		object$cst$chi_sq %>%
			{.["Total"] <- sum(.); .} %>%
			round(2) %>%
			print
	}

	if ("dev_std" %in% check) {
		cat("\nDeviation standardized: (o - e) / sqrt(e)\n")
		print(round(object$cst$residuals, 2)) 	# these seem to be the correct std.residuals
	}

	if ("perc" %in% check) {
		cat("\nProbability table:\n")
		object$cst$observed %>%
			{.["Total"] <- sum(.); .} %>%
			{. / .["Total"]} %>%
			round(2) %>%
			print
	}

	res <- object$cst %>% tidy
	elow <- sum(object$cst$expected < 5)

  if (elow > 0) {
  	res$p.value <- chisq.test(object$cst$observed, simulate.p.value = TRUE, B = 2000) %>% tidy %>% .$p.value
  	res$parameter <- paste0("*",res$parameter,"*")
  }

  round_fun <- function(x) is.na(x) || is.character(x)
	res[!sapply(res, round_fun)] %<>% round(3)

	if (res$p.value < .001) res$p.value  <- "< .001"
	cat(paste0("\nChi-squared: ", res$statistic, " df(", res$parameter, "), p.value ", res$p.value), "\n\n")
	cat(paste(sprintf("%.1f",100 * (elow / length(object$cst$expected))),"% of cells have expected values below 5\n"), sep = "")
	if (elow > 0) cat("p.value for chi-squared statistics obtained using simulation (2000 replicates)")
}

#' Plot method for the goodness function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/goodness} for an example in Radiant
#'
#' @param x Return value from \code{\link{goodness}}
#' @param check Show plots for variable var. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "perc" for percentages
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- goodness("newspaper", "Income")
#' plot(result, check = c("observed","expected","chi_sq"))
#' newspaper %>% goodness("Income") %>% plot(c("observed","expected"))
#'
#' @seealso \code{\link{goodness}} to calculate results
#' @seealso \code{\link{summary.goodness}} to summarize results
#'
#' @export
plot.goodness <- function(x,
                          check = "",
                          shiny = FALSE,
                          ...) {

	object <- x; rm(x)

	return(invisible())

	gather_table <- function(tab) {
		tab %>%
			data.frame(., check.names = FALSE) %>%
			mutate(rnames = rownames(.)) %>%
			{sshhr( gather_(., "variable", "values", setdiff(colnames(.),"rnames")) )}
	}

	plots <- list()

	if ("observed" %in% check) {

		fact_names <- object$cst$observed %>% dimnames %>% as.list
    tab <- as.data.frame(object$cst$observed, check.names = FALSE)
		colnames(tab)[1:2] <- c(object$var1, object$var2)
 	  tab[[1]] %<>% as.factor %>% factor(levels = fact_names[[1]])
		tab[[2]] %<>% as.factor %>% factor(levels = fact_names[[2]])

		plots[['observed']] <-
		  ggplot(tab, aes_string(x = object$var2, y = "Freq", fill = object$var1)) +
		    geom_bar(stat="identity", position = "fill", alpha = .7) +
		    labs(list(title = paste("Observed frequencies for ",object$var2," versus ",object$var1, sep = ""),
				  	 x = object$var2, y = "", fill = object$var1)) +
		    scale_y_continuous(labels = scales::percent)
	}

	if ("expected" %in% check) {
		fact_names <- object$cst$expected %>% dimnames %>% as.list
  	tab <- gather_table(object$cst$expected)
		tab$rnames %<>% as.factor %>% factor(levels = fact_names[[1]])
		tab$variable %<>% as.factor %>% factor(levels = fact_names[[2]])
		plots[['expected']] <-
		  # ggplot(tab, aes_string(x = "rnames", y = "values", fill = "variable")) +
		  ggplot(tab, aes_string(x = "variable", y = "values", fill = "rnames")) +
		    geom_bar(stat="identity", position = "fill", alpha = .7) +
		    labs(list(title = paste("Expected frequencies for ",object$var2," versus ",object$var1, sep = ""),
		         x = object$var2, y = "", fill = object$var1)) +
		    scale_y_continuous(labels = scales::percent)
	}

	if ("chi_sq" %in% check) {
  	tab <- as.data.frame(object$cst$chi_sq, check.names = FALSE)
		colnames(tab)[1:2] <- c(object$var1, object$var2)
		plots[['chi_sq']] <-
		  ggplot(tab, aes_string(x = object$var2, y = "Freq", fill = object$var1)) +
		    geom_bar(stat="identity", position = "dodge", alpha = .7) +
		    labs(list(title = paste("Contribution to chi-squared for ",object$var2," versus ",object$var1, sep = ""),
		         x = object$var2, y = ""))
  }

	if ("dev_std" %in% check) {
  	tab <- as.data.frame(object$cst$residuals, check.names = FALSE)
		colnames(tab)[1:2] <- c(object$var1, object$var2)
		plots[['dev_std']] <-
		  ggplot(tab, aes_string(x = object$var2, y = "Freq", fill = object$var1)) +
		    geom_bar(stat="identity", position = "dodge", alpha = .7) +
		    geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
		    geom_text(data = NULL, x = 1, y = 2.11, label = "95%") +
		    geom_text(data = NULL, x = 1, y = 1.49, label = "90%") +
		    labs(list(title = paste("Deviation standardized for ",object$var2," versus ",object$var1, sep = ""),
		         x = object$var2, y = ""))
	}

	# if ("row_perc" %in% check) {
	# 	plots[['row_perc']] <-
	#   	as.data.frame(object$cst$observed, check.names = FALSE) %>%
	#   	  group_by_("Var1") %>%
	#   	  mutate(perc = Freq / sum(Freq)) %>%
	# 		  ggplot(aes_string(x = "Var2", y = "perc", fill = "Var1")) +
	# 		    geom_bar(stat="identity", position = "dodge", alpha = .7) +
	# 		 		labs(fill = object$var1) +
	# 		 		scale_y_continuous(labels = scales::percent) +
	# 		 		ylab("Percentage") + xlab(object$var2) +
	# 		    ggtitle("Row percentages")
	# }

	# if ("col_perc" %in% check) {
	# 	plots[['col_perc']] <-
	#   	as.data.frame(object$cst$observed, check.names = FALSE) %>%
	#   	  group_by_("Var2") %>%
	#   	  mutate(perc = Freq / sum(Freq)) %>%
	# 		  ggplot(aes_string(x = "Var2", y = "perc", fill = "Var1")) +
	# 		    geom_bar(stat="identity", position = "dodge", alpha = .7) +
	# 		 		labs(fill = object$var1) +
	# 		 		scale_y_continuous(labels = scales::percent) +
	# 		 		ylab("Percentage") + xlab(object$var2) +
	# 		    ggtitle("Column percentages")
	# }

	# if ("perc" %in% check) {
	# 	plots[['perc']] <-
	#   	as.data.frame(object$cst$observed, check.names = FALSE) %>%
	#   	  mutate(perc = Freq / sum(Freq)) %>%
	# 		  ggplot(aes_string(x = "Var2", y = "perc", fill = "Var1")) +
	# 		    geom_bar(stat="identity", position = "dodge", alpha = .7) +
	# 		 		labs(fill = object$var1) +
	# 		 		scale_y_continuous(labels = scales::percent) +
	# 		 		ylab("Percentage") + xlab(object$var2) +
	# 		    ggtitle("Table percentages")
	# }

	sshhr( do.call(gridExtra::arrangeGrob, c(plots, list(ncol = 1))) ) %>%
	  { if (shiny) . else print(.) }
}
