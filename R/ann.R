#' Artificial Neural Networks
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable in the logit (probit) model
#' @param evar Explanatory variables in the model
#' @param lev The level in the response variable defined as _success_
#' @param size Number of units (nodes) in the hidden layer
#' @param decay Paramater decay
#' @param wts Weights to use in estimation
#' @param check Optional output or estimation parameters. "vif" to show the multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates. "odds" to show odds ratios and confidence interval estimates. "standardize" to output standardized coefficient estimates. "stepwise" to apply step-wise selection of variables
#' @param dec Number of decimals to show
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in ann as an object of class ann
#'
#' @examples
#' result <- ann("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' result <- ann("titanic", "survived", c("pclass","sex"))
#'
#' @seealso \code{\link{summary.ann}} to summarize results
#' @seealso \code{\link{plot.ann}} to plot results
#' @seealso \code{\link{predict.ann}} for prediction
#'
#' @importFrom nnet nnet
#'
#' @export
ann <- function(dataset, rvar, evar,
                lev = "",
                size = 1,
                decay = .5,
                wts = "None",
                check = "",
                dec = 3,
                data_filter = "") {

  # dataset <- "titanic"
  # rvar <- "survived"
  # evar <- c("pclass","sex")
  # lev <- "Yes"
  # size <- 1
  # check = ""
  # dec = 3
  # data_filter = ""

  if (rvar %in% evar)
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
           set_class(c("ann",class(.))))

  if (is_empty(size) || size < 1)
    return("Size should be larger than or equal to 1." %>%
           set_class(c("ann",class(.))))

  if (is_empty(decay) || decay < 0)
    return("Decay should be larger than or equal to 0." %>%
           set_class(c("ann",class(.))))


  if (!is.null(wts) && wts == "None") {
    wts <- NULL
    vars <- c(rvar, evar)
  } else {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  # dat <- getdata(dataset, c(rvar, evar), filt = data_filter)
  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  if (!is.null(wts)) {
    wts <- dat[[wtsname]]
    dat <- select_(dat, .dots = paste0("-",wtsname))
  }

  if (any(summarise_each(dat, funs(does_vary)) == FALSE))
    return("One or more selected variables show no variation. Please select other variables." %>%
           set_class(c("ann",class(.))))

  rv <- dat[[rvar]]
  if (lev == "") {
    if (is.factor(rv))
      lev <- levels(rv)[1]
    else
      lev <- rv %>% as.character %>% as.factor %>% levels %>% .[1]
  }

  ## transformation to TRUE/FALSE depending on the selected level (lev)
  dat[[rvar]] <- dat[[rvar]] == lev

  ## stability issues ...
  # http://stats.stackexchange.com/questions/23235/how-do-i-improve-my-neural-network-stability
  scale_df <- function(x) if (is.numeric(x)) scale(x) else x
  dat <- mutate_each_(dat, funs(scale_df), vars = colnames(dat)[-1])

  vars <- evar
  if (length(vars) < (ncol(dat)-1)) vars <- colnames(dat)[-1]

  ## use decay
  # http://stats.stackexchange.com/a/70146/61693

  form <- paste(rvar, "~ . ")
  nninput <- list(formula = as.formula(form),
              rang = .1, size = size, decay = decay, weights = wts, maxit = 10000,
              entropy = TRUE, trace = FALSE, data = dat)

  ## need do.call so Garson plot will work
  model <- do.call(nnet::nnet, nninput)

  rm(dat) ## dat not needed elsewhere

  environment() %>% as.list %>% set_class(c("ann",class(.)))
}

#' Summary method for the ann function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{ann}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", "pclass", lev = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{ann}} to generate esults
#' @seealso \code{\link{plot.ann}} to plot results
#' @seealso \code{\link{predict.ann}} for prediction
#'
#' @export
summary.ann <- function(object, ...) {

  if (is.character(object)) return(object)

  cat("Artificial Neural Network (ANN)\n")
  cat("Activation function: Logistic")
  cat("\nData         :", object$dataset)
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter       :", gsub("\\n","", object$data_filter))
  cat("\nResponse variable    :", object$rvar)
  cat("\nLevel                :", object$lev, "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse=", "),"\n")
  if (length(object$wtsname) > 0)
    cat("Weights used         :", object$wtsname, "\n")
  cat("Nr obs               :", length(object$rv), "\n\n")
  cat("\n")

  print(object$model)
  # print(caret::varImp(object$model))

  if (object$model$convergence != 0)
    cat("\n\nThe model did not converge.")
}

#' Plot method for the ann function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{ann}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' plot(result, plots = c("imp","net"))
#'
#' @seealso \code{\link{ann}} to generate results
#' @seealso \code{\link{summary.ann}} to summarize results
#' @seealso \code{\link{predict.ann}} for prediction
#'
#' @importFrom NeuralNetTools plotnet olden
#'
#' @export
plot.ann <- function(x, shiny = FALSE, ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)
  plot_list <- list()
  plot_list[[1]] <- NeuralNetTools::olden(object$model) + coord_flip()
  # plot_list[[2]] <- NeuralNetTools::garson(object$model) + coord_flip()
  nrCol <- 1

  if (length(plot_list) > 0) {
    sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = nrCol))) ) %>%
      { if (shiny) . else print(.) }
  }
}

#' Predict method for the ann function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{ann}}
#' @param dataset Dataset to use for prediction
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{ann}} to generate results
#' @seealso \code{\link{summary.ann}} to summarize results
#' @seealso \code{\link{plot.ann}} to plot results
#'
#' @export
predict.ann <- function(object, dataset, ...) {
  scale_df <- function(x) if (is.numeric(x)) scale(x) else x

  getdata(dataset, filt = "", na.rm = FALSE) %>%
  mutate_each_(funs(scale_df), vars = colnames(.)[-1]) %>%
  {predict(object$model, .)[,1]}  ## using nnet's predict method
}

#' Store predicted values generated in the ann function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
#'
#' @param pred Return value from predict.nnet
#' @param data Dataset name
#' @param name Variable name assigned to the predicted values
#'
#' @export
store_ann <- function(pred, data,
                      name = "predict_ann") {

  ## fix empty name input
  if (gsub("\\s","",name) == "") name <- "pred_ann"
  changedata(data, vars = pred, var_names = name)
}
