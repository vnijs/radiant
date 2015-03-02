#' Evaluate associations between categorical variables
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/cross_tabs.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param ct_var1 A categorical variable
#' @param ct_var2 Another categorical variable
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param ct_observed Show the observed frequencies table for variables `ct_var1` and `ct_var2`
#' @param ct_expected Show the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds)
#' @param ct_contrib Show the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e)
#' @param ct_std_residuals Show standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e))
#' @param ct_deviation Show the percentage difference between the observed and expected frequencies (i.e., (o - e) / e)
#'
#' @return A list of all variables used in cross_tabs as an object of class cross_tabs
#'
#' @examples
#' result <- cross_tabs("newspaper", "Income", "Newspaper")
#'
#' @seealso \code{\link{summary.cross_tabs}} to summarize results
#' @seealso \code{\link{plot.cross_tabs}} to plot results
#'
#' @export
cross_tabs <- function(dataset, ct_var1, ct_var2,
                     	data_filter = "") {

	dat <- getdata_exp(dataset, c(ct_var1, ct_var2), filt = data_filter)

	dnn = c(paste("Group(",ct_var1,")",sep = ""), paste("Variable(",ct_var2,")",sep = ""))
	tab <- table(dat[,ct_var1], dat[,ct_var2], dnn = dnn)
	cst <- suppressWarnings( chisq.test(tab, correct = FALSE) )

	# dat not needed in summary or plot
	rm(dat)

	# adding the % deviation table
	cst$deviation <- with(cst, (observed-expected) / expected)

	# nrPlot <- sum(c(ct_observed, ct_expected, ct_deviation, ct_std_residuals))
	# nrPlot <- sum(c(ct_expected, ct_deviation, ct_std_residuals))
	# plot_width = 650
	# plot_height = 400 * nrPlot

	time_main <- now()

  environment() %>% as.list %>% set_class(c("cross_tabs",class(.)))
}

#' Summary method for the cross_tabs function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/cross_tabs.html} for an example in Radiant
#'
#' @examples
#' result <- cross_tabs("newspaper", "Income", "Newspaper")
#' summary(result)
#'
#' @seealso \code{\link{cross_tabs}} to calculate results
#' @seealso \code{\link{plot.cross_tabs}} to plot results
#'
#' @export


# rm(r_env)
# result <- cross_tabs("newspaper","Income","Newspaper")
# result

# ct_observed <- TRUE
# summary(result, ct_observed = TRUE)
# plot(result)


# f(result, ct_observed = ct_observed)
# f <- function(result, ...) {
# 	print(list(...))
# 	print()
#   names(list(...)) %>% print
# }


summary.cross_tabs <- function(result, ct_check = "") {

  cat("Time - main",result$time_main,"\n")
  cat("Time - summary",now(),"\n")
  cat("Cross-tabs\n")
	cat("Data     :", result$dataset, "\n")
	if(result$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
	cat("Variables:", paste0(c(result$ct_var1, result$ct_var2), collapse=", "), "\n")
	cat("Null hyp.: there is no association between", result$ct_var1, "and", result$ct_var2, "\n")
	cat("Alt. hyp.: there is an association between", result$ct_var1, "and", result$ct_var2, "\n")

	result$cst$observed %>% rownames %>% c(., "Total") -> rnames
	result$cst$observed %>% colnames %>% c(., "Total") -> cnames

	# if(result$ct_observed) {
	if("observed" %in% ct_check) {
		cat("\nObserved:\n")
		result$cst$observed %>%
			rbind(colSums(.)) %>%
			set_rownames(rnames) %>%
			cbind(rowSums(.)) %>%
			set_colnames(cnames) %>%
			print
	}
	if("expected" %in% ct_check) {
		cat("\nExpected:\n")
		result$cst$expected %>%
			rbind(colSums(.)) %>%
			set_rownames(rnames) %>%
			cbind(rowSums(.)) %>%
			set_colnames(cnames) %>%
			round(2) %>%
			print
	}
	# if(result$ct_contrib) {
	if("chi" %in% ct_check) {
		cat("\nContribution to chi-squared:\n")
		# print((result$cst$observed - result$cst$expected)^2 / result$cst$expected, digits = 2)
		((result$cst$observed - result$cst$expected)^2 / result$cst$expected) %>%
			rbind(colSums(.)) %>%
			set_rownames(rnames) %>%
			cbind(rowSums(.)) %>%
			set_colnames(cnames) %>%
			round(2) %>%
			print
	}
	# if(result$ct_std_residuals) {
	if("dev_std" %in% ct_check) {
		cat("\nDeviation standardized:\n")
		print(round(result$cst$residuals, 2)) 	# these seem to be the correct std.residuals
	}
	# if(result$ct_deviation) {
	if("dev_perc" %in% ct_check) {
		cat("\nDeviation %:\n")
		print(round(result$cst$deviation, 2)) 	# % deviation
	}
	# if(result$ct_cellperc) {
	# 	cat("\nCell percentages:\n")
	# 	print(prop.table(result$table), digits = 2)  	# cell percentages
	# }
	# if(result$ct_rowperc) {
	# 	cat("\nRow percentages:\n")
	# 	print(prop.table(result$table, 1), digits = 2) # row percentages
	# }
	# if(result$ct_colperc) {
	# 	cat("\nColumn percentages:\n")
	# 	print(prop.table(result$table, 2), digits = 2) # column percentages
	# }

	result$cst %>% tidy %>% round(3) -> res
	if(res$p.value < .001) res$p.value  <- "< .001"
	cat(paste0("\nChi-squared: ", res$statistic, " df(", res$parameter, "), p.value ", res$p.value), "\n\n")

	cat(paste(sprintf("%.1f",100 * (sum(result$cst$expected < 5) / length(result$cst$expected))),"% of cells have expected values below 5\n\n"), sep = "")
}

#' Plot method for the cross_tabs function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/cross_tabs.html} for an example in Radiant
#'
#' @examples
#' result <- cross_tabs("newspaper", "Income", "Newspaper", ct_expected = TRUE)
#' plot(result)
#'
#' @seealso \code{\link{cross_tabs}} to calculate results
#' @seealso \code{\link{summary.cross_tabs}} to summarize results
#'
#' @export
plot.cross_tabs <- function(result, ct_check = "") {

	gather_table <- function(tab) {
		tab %>%
			data.frame(., check.names = FALSE) %>%
			mutate(rnames = rownames(.)) %>%
			{ suppressMessages( gather_(., "variable", "values") ) }
	}

	plots <- list()


	# if(result$ct_std_residuals) {
	if("dev_std" %in% ct_check) {

		tab <- gather_table(result$cst$residuals)
		colnames(tab)[1:2] <- c(result$ct_var1, result$ct_var2)
		plots[['residuals']] <- ggplot(tab, aes_string(x = result$ct_var1, y = "values", fill = result$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) +
     					geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
     					geom_text(data = NULL, x = 1, y = 2.11, label = "95%") +
     					geom_text(data = NULL, x = 1, y = 1.49, label = "90%") +
         			labs(list(title = paste("Deviation standardized for ",result$ct_var2," versus ",result$ct_var1, sep = ""), x = result$ct_var1))
	}

	# if(result$ct_deviation) {
	if("dev_perc" %in% ct_check) {

		tab <- gather_table(result$cst$deviation)
		colnames(tab)[1:2] <- c(result$ct_var1, result$ct_var2)
		plots[['deviation']] <- ggplot(tab, aes_string(x = result$ct_var1, y = "values", fill = result$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) + ylim(-1,1) +
         			labs(list(title = paste("Deviation % for ",result$ct_var2," versus ",result$ct_var1, sep = ""), x = result$ct_var1))
	}

	if("chi" %in% ct_check) {
		plot(1:10)
	}

	# if(result$ct_expected) {
	if("expected" %in% ct_check) {

		fact_names <- result$cst$expected %>% dimnames %>% as.list
  	tab <- gather_table(result$cst$expected)
		tab$rnames %<>% as.factor %>% factor(levels = fact_names[[1]])
		tab$variable %<>% as.factor %>% factor(levels = fact_names[[2]])
		plots[['expected']] <- ggplot(tab, aes_string(x = "rnames", y = "values", fill = "variable")) +
         			geom_bar(stat="identity", position = "fill", alpha = .7) +
         			labs(list(title = paste("Expected frequencies for ",result$ct_var2," versus ",result$ct_var1, sep = ""),
							x = "", y = "", fill = result$ct_var2))
	}

	# if(result$ct_observed) {
	if("observed" %in% ct_check) {

		fact_names <- result$cst$observed %>% dimnames %>% as.list
  	tab <- gather_table(result$cst$observed)
		colnames(tab)[1:2] <- c(result$ct_var1, result$ct_var2)
		tab$result$ct_var1 %<>% as.factor %>% factor(levels = fact_names[[1]])
		tab$result$ct_var1 %<>% as.factor %>% factor(levels = fact_names[[2]])

		plots[['stacked']] <-
		ggplot(tab, aes_string(x = result$ct_var1, y = "values", fill = result$ct_var2)) +
         			geom_bar(stat="identity", position = "fill", alpha = .7) +
         			labs(list(title = paste("Observed frequencies for ",result$ct_var2," versus ",result$ct_var1, sep = ""),
							x = "", y = "", fill = result$ct_var2))
	}

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}
