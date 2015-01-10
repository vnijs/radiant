###############################
# Single mean
###############################

# single_mean <- function(dataset, sm_var, sm_comp_value = 0,
single_mean <- function(dataset, sm_var,
                        data_set_filter = "",
                        sm_comp_value = 0,
                        sm_alternative = 'two.sided',
                        sm_sig_level = .95) {




  # argg <- c(as.list(environment()), list(...))
  # argg <- c(as.list(environment()))
  # print(argg)




	dat <- values[[dataset]][,sm_var]

	# dat_filter <- values[[dataset_filter]]

	res <- t.test(dat, mu = sm_comp_value, alternative = sm_alternative,
		conf.level = sm_sig_level)

  data.frame(dat) %>%
  	set_names(sm_var) %>%
  	list("dataset" = dataset, dat = .) -> xtra




  # print(as.list(match.call()))


  # if you can pass the full function call then perhaps you should
  # do broom::tidy here. That way you can use the function call
  # names in the summary_ and plots_ which should make the code easier
  # to read



	list(res = res, xtra = xtra)
}

# values <- list()
# values$mtcars <- mtcars
# result <- single_mean("mtcars", "mpg")
# summary_single_mean(result)
# result <- single_mean("mtcars", "mpg", sm_comp_value = 10)
# summary_single_mean(result)
# result <- single_mean("mtcars", "mpg", sm_comp_value = 10,
#                       sm_alternative = 'less', sm_sig_level = .90)
# summary_single_mean(result)



summary_single_mean <- function(result = .single_mean()) {
	res <- result$res
	dat <- result$xtra$dat
	res$data.name <- names(dat)
	cat("Data     :", result$xtra$dataset, "\n")

	# cat("Filter   :", result$xtra$filter, "\n")

	cat("Variable :", res$data.name, "\n")

	if (res$alternative == "two.sided") {
		hyp_symbol <- "not equal to"
	} else if (res$alternative == "less") {
		hyp_symbol <- "<"
	} else {
		hyp_symbol <- ">"
	}

	cat("Null hyp.: the mean of", res$data.name, "=", res$null.value, "\n")
	cat("Alt. hyp.: the mean of", res$data.name, "is", hyp_symbol, res$null.value, "\n\n")

	conf.int <- res$conf.int[1:2]
	conf.level <- attr(res$conf.int,"conf.level")
	conf.perc <- 100 * (1-conf.level)/2
	conf.perc <- c(conf.perc, 100 - conf.perc) %>% round(1) %>% paste0(.,"%")

	tidy_res <- tidy(res)
	names(tidy_res) <- c("mean","t.value","p.value","df", conf.perc[1],
	                     conf.perc[2])

	p.value <- round(tidy_res$p.value,3)
	if (p.value < .001) p.value <- "< .001"
	tidy_res$p.value <- p.value
	tidy_res$n <- nrow(dat)
	tidy_res$sd <- sd(dat[,1])

	print(tidy_res, row.names = FALSE)
}

plots_single_mean <- function(result = .single_mean()) {

	dat <- result$xtra$dat
	bw <- diff(range(dat, na.rm = TRUE)) / 12
	res <- result$res
	p <- ggplot(dat, aes_string(x=names(dat))) +
		geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
		geom_vline(xintercept = c(res$null.value), color = 'red', linetype = 'longdash', size = 1) +
		geom_vline(xintercept = res$estimate, color = 'black', linetype = 'solid', size = 1) +
		geom_vline(xintercept = res$conf.int, color = 'black', linetype = 'longdash', size = .5)
	print(p)
}
