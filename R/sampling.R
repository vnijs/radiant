#' Simple random sampling
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sampling.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var The variable to sample from
#' @param sample_size Number of units to select
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of variables defined in sampling as an object of class sampling
#'
#' @examples
#' result <- sampling("rndnames","Names",10)
#'
#' @seealso \code{\link{summary.sampling}} to summarize results
#' @export
sampling <- function(dataset, var, sample_size,
                     data_filter = "") {

  dat <- getdata(dataset, var, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

	## example list of names obtained from http://listofrandomnames.com
  dat$rnd_number <- runif(nrow(dat), min = 0, max = 1) %>% round(3)
  dat %>%
    arrange(desc(rnd_number)) %>%
    slice(1:sample_size) -> seldat

  environment() %>% as.list %>% set_class(c("sampling",class(.)))
}

#' Summary method for the sampling function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sampling} for an example in Radiant
#'
#' @param object Return value from \code{\link{sampling}}
#' @param print_sf Print full sampling frame. Default is TRUE
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' set.seed(1234)
#' result <- sampling("rndnames", "Names", 10)
#' summary(result)
#'
#' @seealso \code{\link{sampling}} to generate the results
#'
#' @export
summary.sampling <- function(object,
                             print_sf = TRUE,
                             ...) {

  cat("Sampling (simple random)\n")
  cat("Data       :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter     :", gsub("\\n","", object$data_filter), "\n")
  cat("ID variable:", object$var, "\n")
  cat("Sample size:", object$sample_size, "\n\n")
  cat("Selected:\n")
	print(object$seldat, row.names = FALSE)
  if (print_sf) {
    cat("\nSampling frame:\n")
    print(object$dat, row.names = FALSE)
  }
}
