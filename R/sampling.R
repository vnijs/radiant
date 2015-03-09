#' Simple random sampling
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/sampling.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param smp_var The variable to sample from
#' @param smp_sample_size Number of units to select
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param smp_print_full Print full sampling frame. Default is TRUE
#'
#' @return A list of variables defined in sampling as an object of class sampling
#'
#' @examples
#' result <- sampling("rndnames","Names",10)
#'
#' @seealso \code{\link{summary.sampling}} to summarize results
#' @export
sampling <- function(dataset, smp_var, smp_sample_size,
                     data_filter = "",
                     smp_print_full = TRUE) {

  dat <- getdata(dataset, smp_var, filt = data_filter)

	# example list of names obtained from http://listofrandomnames.com
  dat$rnd_number <- runif(nrow(dat), min = 0, max = 1)
  dat %>%
    arrange(desc(rnd_number)) %>%
    slice(1:smp_sample_size) -> seldat

  environment() %>% as.list %>% set_class(c("sampling",class(.)))
}


#' Summary method for sampling
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/sampling} for an example in Radiant
#'
#' @param result Return value from \code{\link{sampling}}
#'
#' @examples
#' result <- sampling("rndnames","Names",10)
#' summary(result)
#'
#' @seealso \code{\link{sampling}} to generate the results
#'
#' @export
summary.sampling <- function(result) {
  cat("Sampling (simple random)\n")
  cat("Data       :", result$dataset, "\n")
  if(result$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter     :", gsub("\\n","", result$data_filter), "\n")
  cat("ID variable:", result$smp_var, "\n")
  cat("Sample size:", result$smp_sample_size, "\n\n")

  cat("Selected:\n")
	print(result$seldat)
	cat("\nSampling frame (max 100 shown):\n")
  if(result$smp_print_full)
    head(result$dat, n = 100) %>% print
}
