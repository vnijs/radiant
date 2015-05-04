#' Explore data
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param expl_vars (Numerical) variables to summaries
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param expl_byvar Variable(s) to group data by before summarizing
#' @param expl_fun Functions to use for summarizing
#'
#' @return A list of all variables defined in the function as an object of class explore
#'
#' @examples
#' result <- explore("diamonds", "price:x")
#' summary(result)
#' result <- explore("diamonds", "price", expl_byvar = "cut", expl_fun = c("length", "skew"))
#' summary(result)
#'
#' @seealso \code{\link{summary.explore}} to show summaries
#' @seealso \code{\link{plot.explore}} to plot summaries
#'
#' @export
explore <- function(dataset, expl_vars = "",
                    data_filter = "",
                    expl_byvar = "",
                    expl_fun = c("length", "mean_rm")) {

  vars <- expl_vars
  if (!is_empty(expl_byvar))
    vars %<>% c(expl_byvar)

  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  # in case : was used
  expl_vars <- colnames(head(dat) %>% select_(.dots = expl_vars))

  if (is_empty(expl_byvar)) {
    res <- capture.output(getsummary(dat))
  } else {
    res <- list()
    dc <- getclass(dat)
    isNum <- "numeric" == dc | "integer" == dc
    dat %<>% group_by_(.dots = expl_byvar) %>% select(which(isNum))
    for (f in expl_fun) {
      gf <- get(f)
      res[[f]] <- dat %>% summarise_each(funs(gf)) %>% as.data.frame
    }
  }

  # dat no longer needed
  rm(dat)

  environment() %>% as.list %>% set_class(c("explore",class(.)))
}

#' Summary method for the explore function
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{explore}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- explore("diamonds", "price:x")
#' summary(result)
#' result <- explore("diamonds", "price", expl_byvar = "cut", expl_fun = c("length", "skew"))
#' summary(result)
#' diamonds %>% explore("price:x") %>% summary
#' diamonds %>% explore("price", expl_byvar = "cut", expl_fun = c("length", "skew")) %>% summary
#'
#' @seealso \code{\link{explore}} to generate summaries
#' @seealso \code{\link{plot.explore}} to plot summaries
#'
#' @export
summary.explore <- function(object, ...) {

  cat("Data     :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter   :", gsub("\\n","", object$data_filter), "\n")

  cat("\n")
  if (class(object$res) == "character") {
    cat(paste0(object$res[-length(object$res)],sep="\n"))
  } else {

    if (!exists("expl_functions"))
      funcs <- object$expl_fun %>% set_names(.,.)
    else
      funcs <- get("expl_functions")

    for (f in object$expl_fun) {
      cat("Results grouped by: ", object$expl_byvar, "\n")
      cat("Function used: ", names(which(funcs == f)), "\n")
      object$res[[f]] %>%
        { .[,-c(1:length(object$expl_byvar))] %<>% round(3); . } %>%
        print
      cat("\n")
    }
  }

  invisible()
}

#' Plot method for the explore function
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant. A plot will only be generated when a 'by' variable has been specified
#'
#' @param x Return value from \code{\link{explore}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- explore("diamonds", "price", expl_byvar = "cut", expl_fun = c("length", "skew"))
#' plot(result)
#'
#' @seealso \code{\link{explore}} to generate summaries
#' @seealso \code{\link{summary.explore}} to show summaries
#'
#' @export
plot.explore <- function(x, shiny = FALSE, ...) {

  object <- x; rm(x)

  if (class(object$res)[1] == "character")
    return(invisible())

  by_var <- fill_var <- object$expl_byvar[1]
  if (length(object$expl_byvar) > 1) fill_var <- object$expl_byvar[2]

  if (!exists("expl_functions")) {
    funcs <- object$expl_fun %>% set_names(.,.)
  } else {
    funcs <- get("expl_functions")
  }

  plots <- list()
  for (f in object$expl_fun) {
    for (var in object$expl_vars) {
      plots[[paste0(var,"_",f)]] <-
        ggplot(data = object$res[[f]], aes_string(x = by_var, y = var, fill = fill_var)) +
          geom_bar(stat="identity", position = "dodge", alpha=.7) +
          ggtitle(paste("Function used:", names(which(funcs == f))))

      if (length(object$expl_byvar) == 1) {
        plots[[paste0(var,"_",f)]] <- plots[[paste0(var,"_",f)]] +
          theme(legend.position = "none")
      }
    }
  }

  sshhr( do.call(gridExtra::arrangeGrob, c(plots, list(ncol = 1))) ) %>%
    { if (shiny) . else print(.) }
}

#' Create data.frame summary
#'
#' @details Used by Explore and Transform
#'
#' @param dat Data.frame
#' @param dc Class for each variable
#'
#' @export
getsummary <- function(dat, dc = getclass(dat)) {

  isFct <- "factor" == dc
  isNum <- "numeric" == dc | "integer" == dc
  isDate <- "date" == dc
  isChar <- "character" == dc
  isLogic <- "logical" == dc

  if (sum(isNum) > 0) {

    cn <- names(dc)[isNum]

    cat("Summarize numeric variables:\n")
    select(dat, which(isNum)) %>%
      tidyr::gather_("variable", "values", cn) %>%
      group_by_("variable") %>%
      summarise_each(funs(n = length, missing = nmissing, mean = mean_rm,
                     median = median_rm, min = min_rm, max = max_rm,
                     `25%` = p25, `75%` = p75, sd = sd_rm, se = serr,
                     cv = sd/mean)) %>%
      data.frame(check.names = FALSE) %>%
      { .[,-1] %<>% round(.,3); colnames(.)[1] <- ""; . } %>%
      print(row.names = FALSE)
    cat("\n")
  }
  if (sum(isFct) > 0) {
    cat("Summarize factors:\n")
    select(dat, which(isFct)) %>% summary %>% print
    cat("\n")
  }
  if (sum(isDate) > 0) {
    cat("Earliest dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(min)) %>% print
    cat("\nFinal dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(max)) %>% print
    cat("\n")
  }
  if (sum(isChar) > 0) {
    cat("Summarize character variables:\n")
    select(dat, which(isChar)) %>% table %>% print
    cat("\n")
  }
  if (sum(isLogic) > 0) {
    cat("Summarize logical variables:\n")
    select(dat, which(isLogic)) %>% tally %>% print
    cat("\n")
  }
}

###########################################
## turn functions below into functional ...
###########################################

#' Number of missing values
#' @param x Input variable
#' @return number of missing values
#' @examples
#' nmissing(c("a","b",NA))
#'
#' @export
nmissing <- function(x) sum(is.na(x))

#' 25th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 25th percentile
#' @examples
#' p25(rnorm(100))
#'
#' @export
p25 <- function(x, na.rm = TRUE) quantile(x,.25, na.rm = na.rm)

#' 75th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 75th percentile
#' @examples
#' p75(rnorm(100))
#'
#' @export
p75 <- function(x, na.rm = TRUE) quantile(x,.75, na.rm = na.rm)

#' Standard error
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Standard error
#' @examples
#' serr(rnorm(100))
#'
#' @export
serr <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / length(na.omit(x))

#' Coefficient of variation
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Coefficient of variation
#' @examples
#' cv(runif (100))
#'
#' @export
cv <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)

#' Mean with na.rm = TRUE
#' @param x Input variable
#' @return Mean value
#' @examples
#' mean_rm(runif (100))
#'
#' @export
mean_rm <- function(x) mean(x, na.rm = TRUE)

#' Median with na.rm = TRUE
#' @param x Input variable
#' @return Median value
#' @examples
#' median_rm(runif (100))
#'
#' @export
median_rm <- function(x) median(x, na.rm = TRUE)

#' Min with na.rm = TRUE
#' @param x Input variable
#' @return Minimum value
#' @examples
#' min_rm(runif (100))
#'
#' @export
min_rm <- function(x) min(x, na.rm = TRUE)

#' Max with na.rm = TRUE
#' @param x Input variable
#' @return Maximum value
#' @examples
#' max_rm(runif (100))
#'
#' @export
max_rm <- function(x) max(x, na.rm = TRUE)

#' Standard deviation with na.rm = TRUE
#' @param x Input variable
#' @return Standard deviation
#' @examples
#' sd_rm(rnorm(100))
#'
#' @export
sd_rm <- function(x) sd(x, na.rm = TRUE)

