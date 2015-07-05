#' Simulate data for decision analysis
#'
#' @details See \url{http://vnijs.github.io/radiant/base/simulater.html} for an example in Radiant
#'
#' @param const A string listing the constants to include in the analysis (e.g., "cost = 3\\nsize = 4")
#' @param norm A string listing the normally distributed random variables to include in the analysis (e.g., "demand = c(2000,1000)" where the first number is the mean and the second is the standard deviation)
#' @param unif A string listing the uniformly distributed random variables to include in the analysis (e.g., "demand = c(0,1)" where the first number is the minimum value and the second is the maximum value)
#' @param discrete A string listing the random variables with a discrete distribution to include in the analysis (e.g., "price = c(5, .3, 8, .7)" where for each pair of numbers the first is the value and the second the probability
#' @param form A string for the formula to evaluate (e.g., "profit = demand * (price - cost)")
#'
#' @return A data.frame with the created variables
#'
#' @examples
#' result <- simulater(const = "cost = 3", norm = "demand = c(2000,1000)",
#'                     discrete = "price = c(5, .3, 8, .7)",
#'                     form = "profit = demand * (price - cost)")
#'
#' @seealso \code{\link{summary.simulater}} to summarize results
#' @seealso \code{\link{plot.simulater}} to plot results
#' @export
simulater <- function(const = "",
                      norm = "",
                      unif = "",
                      discrete = "",
                      form = "",
                      seed = "1234",
                      name = "",
                      nr = 1000) {

  # rm(list = ls())
  # const <- "non_labor_cost = 3995\ncost = 11"
  # unif <- "labor_cost = c(5040, 6860)"
  # discrete <- "price = c(20, .25, 18.5, .35, 16.5, .3, 15, .1)"
  # norm <- "nr_meals = c(3000, 1000)"
  # form <- "profit = (price - cost)*nr_meals - labor_cost - non_labor_cost"
  # seed <- 1234
  # name <- "sim1"
  # nr <- 1000

  ## remove any non-numbers from seed, including spaces
  seed %>% gsub("[^0-9]","",.) %>% { if(. != "") set.seed(seed) }
  dat <- list()

  cleaner <- function(x) x %>% gsub(" ","",.) %>% gsub("[\n]+","\n",.) %>% gsub("\n$","",.)
  spliter <- function(x) x %>% strsplit(., "\n") %>% extract2(1) %>% strsplit(., "=")

  ## parsing constant
  # const <- "non_labor_cost = 3995\n\ncost = 11\n   \n \n\n\n\n\n   "
  const %<>% cleaner
  if(const != "") {
    s <- const %>% spliter
    for(i in 1:length(s))
      dat[[s[[i]][1]]] <- as.numeric(s[[i]][2]) %>% rep(,nr)
  }

  ## parsing uniform
  unif %<>% cleaner
  if(unif != "") {
    s <- unif %>% spliter
    for(i in 1:length(s)) {
      par <- s[[i]][2] %>% sub("c\\(","",.) %>% sub(")","",.) %>% strsplit(.,",") %>%
        extract2(1) %>% as.numeric
      dat[[s[[i]][1]]] <- runif(nr, par[1], par[2])
    }
  }

  ## parsing normal
  norm %<>% cleaner
  if(norm != "") {
    # s <- norm %>% strsplit(., "\n") %>% extract2(1) %>% strsplit(., "=")
    s <- norm %>% spliter
    for(i in 1:length(s)) {
      par <- s[[i]][2] %>% sub("c\\(","",.) %>% sub(")","",.) %>% strsplit(.,",") %>%
        extract2(1) %>% as.numeric
      # assign(s[[i]][1], rnorm(nr, par[1], par[2]))
      dat[[s[[i]][1]]] <- rnorm(nr, par[1], par[2])
    }
  }

  ## parsing discrete
  discrete %<>% cleaner
  if(discrete != "") {
    s <- discrete %>% spliter
    for(i in 1:length(s)) {
      par <- s[[i]][2] %>% sub("c\\(","",.) %>% sub(")","",.) %>% strsplit(.,",") %>%
        extract2(1) %>% as.numeric %>% matrix(nrow = 2)
      dat[[s[[i]][1]]] <- sample(par[1,], nr, replace = TRUE, prob = par[2,])
    }
  }

  ## parsing formula
  form %<>% cleaner
  if(form != "") {
    s <- form %>% spliter
    for(i in 1:length(s)) {
      objective <- s[[i]][1]
      out <- try(do.call(with, list(dat, parse(text = s[[i]][2]))), silent = TRUE)
      if (!is(out, 'try-error')) {
        dat[[objective]] <- out
      } else {
        dat[[objective]] <- NA
        message("Formula", s[[i]][2], "was not successfully evaluated")
      }
    }
  }

  dat %<>% as.data.frame %>% set_class(c("simulater", class(.)))

  name %<>% gsub(" ","",.)
  if (name != "") {
    if (exists("r_env")) {
      env <- r_env
    } else if (exists("r_data")) {
      env <- pryr::where("r_data")
    } else {
      return(dat)
    }

    mess <- paste0("\n### Simulated data\n\nFormula: ", form, "\n\nOn: ",
                   lubridate::now())

    env$r_data[[name]] <- dat
    env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique
    env$r_data[[paste0(name,"_descr")]] <- mess
  }

  dat
}

#' Summary method for the simulater function
#'
#' @details See \url{http://vnijs.github.io/radiant/base/simulater.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{simulater}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- simulater(norm = "demand = c(2000,1000)")
#' summary(result)
#'
#' @seealso \code{\link{simulater}} to generate the results
#' @seealso \code{\link{plot.simulater}} to plot results
#'
#' @export
summary.simulater <- function(object, ...) {
  ## show results
  # print(head(object)); cat("\n")
  getsummary(object)
}

#' Plot method for the simulater function
#'
#' @details See \url{http://vnijs.github.io/radiant/base/simulater} for an example in Radiant
#'
#' @param x Return value from \code{\link{simulater}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- simulater(const = "cost = 3", norm = "demand = c(2000,1000)",
#'                     discrete = "price = c(5, .3, 8, .7)",
#'                     form = "profit = demand * (price - cost)")
#' plot(result)
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{summary.single_mean}} to summarize results
#'
#' @export
plot.simulater <- function(x, shiny = FALSE, ...) {

  object <- x; rm(x)

  plots <- list()
  for (i in colnames(object)) {

    dat <- select_(object, .dots = i)
    if (sd(object[[i]]) == 0) {
      dat$sim <- 1:nrow(dat)
      plots[[i]] <- ggplot(dat, aes_string(x = "sim", y = i)) +
        geom_line(color = "blue")
      next
    }

    bw <- diff(range(dat[[1]], na.rm = TRUE)) / 20

    ## plot results
    plots[[i]] <- ggplot(dat, aes_string(x = i)) +
      geom_histogram(aes(y = ..density..), binwidth = bw, alpha = .3) +
      geom_density(adjust=1.5, color = "blue", alpha=.3)
  }

  sshhr( do.call(gridExtra::arrangeGrob, c(plots, list(ncol = min(length(plots),2)))) ) %>%
    { if (shiny) . else print(.) }
}
