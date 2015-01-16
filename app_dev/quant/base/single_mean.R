###############################
# Single mean
###############################
single_mean <- function(dataset, sm_var,
                        dataset_filter = "",
                        sm_comp_value = 0,
                        sm_alternative = "two.sided",
                        sm_sig_level = .95) {

	if(exists("values"))
		dat <- select_(values[[dataset]], sm_var)
		# if a data_filter has been defined
		# dat <- filter(dat, values[[dataset_filter]])
	else
		dat <- select_(get(dataset), sm_var)

	t.test(dat, mu = sm_comp_value, alternative = sm_alternative,
	       conf.level = sm_sig_level) %>% tidy -> res

  # as.list(environment())
  environment() %>% as.list
}

summary_single_mean <- function(result = .single_mean()) {

	cat("Data     :", result$dataset, "\n")
	# cat("Filter   :", result$xtra$filter, "\n")
	cat("Variable :", result$sm_var, "\n")

	if (result$sm_alternative == "two.sided") {
		hyp_symbol <- "not equal to"
	} else if (result$sm_alternative == "less") {
		hyp_symbol <- "<"
	} else {
		hyp_symbol <- ">"
	}

	cat("Null hyp.: the mean of", result$sm_var, "=", result$sm_comp_value, "\n")
	cat("Alt. hyp.: the mean of", result$sm_var, "is", hyp_symbol,
	    result$sm_comp_value, "\n\n")

	# determine lower and upper % for ci
	{100 * (1-result$sm_sig_level)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%") -> ci_perc

	result$res$n <- nrow(result$dat)
	result$res$sd <- sd(result$dat[,result$sm_var])
	res <- round(result$res, 3) 	# restrict to 3 decimal places
	names(res)[1:6] <- c("mean","t.value","p.value","df", ci_perc[1], ci_perc[2])
	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

plots_single_mean <- function(result = .single_mean()) {

	bw <- diff(range(result$dat, na.rm = TRUE)) / 12
	res <- result$res
	p <- ggplot(result$dat, aes_string(x=result$sm_var)) +
		geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
		geom_vline(xintercept = result$sm_comp_value, color = 'red',
		           linetype = 'longdash', size = 1) +
		geom_vline(xintercept = result$res$estimate, color = 'black',
		           linetype = 'solid', size = 1) +
		geom_vline(xintercept = c(result$res$conf.low, result$res$conf.high),
		           color = 'black', linetype = 'longdash', size = .5)
	print(p)
}
