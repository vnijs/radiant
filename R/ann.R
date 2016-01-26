#' Artificial Neural Networks
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable in the logit (probit) model
#' @param evar Explanatory variables in the model
#' @param lev The level in the response variable defined as _success_
#' @param size Number of units (nodes) in the hidden layer
#' @param decay Paramter decay
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
#' @seealso \code{\link{summary.ann}} to summarize the results
#' @seealso \code{\link{plot.ann}} to plot the results
#' @seealso \code{\link{predict.ann}} to generate predictions
#' @seealso \code{\link{plot.ann_predict}} to plot prediction output
#'
#' @importFrom nnet nnet
#'
#' @export
ann <- function(dataset, rvar, evar,
                lev = "",
                size = 1,
                decay = .5,
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

  dat <- getdata(dataset, c(rvar, evar), filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

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

  vars <- evar
  if (length(vars) < (ncol(dat)-1)) vars <- colnames(dat)[-1]

  ## use decay
  # http://stats.stackexchange.com/a/70146/61693

  form <- paste(rvar, "~ . ")
  nninput <- list(formula = as.formula(form),
              rang = .01, size = size, decay = decay, maxit = 5000,
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
#' @param sum_check Optional output or estimation parameters. "rsme" to show the root mean squared error. "sumsquares" to show the sum of squares table. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", "pclass", lev = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{ann}} to generate the results
#' @seealso \code{\link{plot.ann}} to plot the results
#' @seealso \code{\link{predict.ann}} to generate predictions
#' @seealso \code{\link{plot.ann_predict}} to plot prediction output
#'
#' @export
summary.ann <- function(object,
                        sum_check = "",
                        ...) {

  if (is.character(object)) return(object)
  # if (class(object$model)[1] != 'ann') return(object)
  # dec <- object$dec

  cat("Artificial Neural Network (ANN)\n")
  cat("Activation function: Logistic")
  cat("\nData         :", object$dataset)
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter       :", gsub("\\n","", object$data_filter))
  cat("\nResponse variable    :", object$rvar)
  cat("\nLevel                :", object$lev, "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse=", "),"\n")
  cat("Nr obs               :", length(object$rv), "\n\n")
  cat("\n")

  print(object$model)

  if (object$model$convergence != 0)
    cat("\n\nThe model did not converge.")
}

#' Plot method for the ann function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{ann}}
#' @param plots Plots to produce for the specified ANN model. Use "" to avoid showing any plots (default). "imp" shows variable importance. "net" shows the network
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' plot(result, plots = c("imp","net"))
#'
#' @seealso \code{\link{ann}} to generate results
#' @seealso \code{\link{plot.ann}} to plot results
#' @seealso \code{\link{predict.ann}} to generate predictions
#'
#' @importFrom NeuralNetTools plotnet garson
#'
#' @export
plot.ann <- function(x,
                     plots = "",
                     shiny = FALSE,
                     ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)
  plot_list <- list()
  plot_list[[1]] <- NeuralNetTools::garson(object$model)
  nrCol <- 1

  # if ("hist" %in% plots) {}

  if (length(plot_list) > 0) {
    sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = nrCol))) ) %>%
      { if (shiny) . else print(.) }
  }
}

# #' Predict method for the ann function
# #'
# #' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
# #'
# #' @param object Return value from \code{\link{ann}}
# #' @param pred_vars Variables selected to generate predictions
# #' @param pred_data Provide the name of a dataframe to generate predictions (e.g., "titanic"). The dataset must contain all columns used in the estimation
# #' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable use a `,` (e.g., `pclass = levels(pclass), age = seq(0,100,20)`)
# #' @param prn Print prediction results (default is TRUE)
# #' @param ... further arguments passed to or from other methods
# #'
# #' @examples
# #' result <- ann("titanic", "survived", c("pclass","sex"), lev = "Yes")
# #'  predict(result, pred_cmd = "pclass = levels(pclass)")
# #' ann("titanic", "survived", c("pclass","sex"), lev = "Yes") %>%
# #'   predict(pred_cmd = "sex = c('male','female')")
# #' ann("titanic", "survived", c("pclass","sex"), lev = "Yes") %>%
# #'  predict(pred_data = "titanic")
# #'
# #' @seealso \code{\link{ann}} to generate the result
# #' @seealso \code{\link{summary.ann}} to summarize results
# #' @seealso \code{\link{plot.ann}} to plot results
# #' @seealso \code{\link{plot.ann_predict}} to plot prediction output
# #'
# #' @export
# predict.ann <- function(object,
#                             pred_vars = "",
#                             pred_data = "",
#                             pred_cmd = "",
#                             prn = TRUE,
#                             ...) {

#   if (is.character(object)) return(object)
#   pred_count <- sum(c(pred_vars == "", pred_cmd == "", pred_data == ""))
#   ## used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression as starting point
#   if ("standardize" %in% object$check) {
#     return(cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the standardized coefficients box and try again"))
#   } else if (pred_count == 3) {
#     return(cat("Please specify a command to generate predictions. For example,\n pclass = levels(pclass) would produce predictions for the different\n levels of factor pclass. To add another variable use a ,\n(e.g., pclass = levels(pclass), age = seq(0,100,20))\n\nMake sure to press return after you finish entering the command. If no\nresults are shown the command was invalid. Alternatively specify a dataset\nto generate predictions. You could create this in a spreadsheet and use the\nclipboard feature in Data > Manage to bring it into Radiant"))
#   }

#   dec <- object$dec

#   if (pred_count < 2) {
#     if (pred_cmd != "")
#       cat("Multiple inputs where specified for prediciton. The command will be used.\nTo use variables or a dataset remove the command.")
#     if (pred_vars != "")
#       cat("Multiple inputs where specified for prediciton. The variables selected will be used.\nTo use a command or dataset unselect variables.")
#   }

#   pred_type <- "cmd"
#   vars <- object$evar
#   if (pred_cmd != "") {
#     pred_cmd %<>% gsub("\"","\'", .) %>% gsub(";",",", .)
#     pred <- try(eval(parse(text = paste0("with(object$model$model, expand.grid(", pred_cmd ,"))"))), silent = TRUE)
#     if (is(pred, 'try-error')) {
#       paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred,"condition")$message, "\n\nPlease try again. Examples are shown in the help file.") %>% cat
#       return()
#     }

#     ## adding information to the prediction data.frame
#     dat_classes <- attr(object$model$term, "dataClasses")[-1]
#     isFct <- dat_classes == "factor"
#     isNum <- dat_classes == "numeric"
#     dat <- select_(object$model$model, .dots = vars)

#     ## based on http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
#     max_freq <- function(x) names(which.max(table(x)))

#     plug_data <- data.frame(init___ = 1)
#     if (sum(isNum) > 0)
#       plug_data %<>% bind_cols(., summarise_each_(dat, funs(mean), vars[isNum]))
#     if (sum(isFct) > 0)
#       plug_data %<>% bind_cols(., summarise_each_(dat, funs(max_freq), vars[isFct]))

#     rm(dat)

#     if (sum(isNum) + sum(isFct) < length(vars)) {
#       cat("The model includes data-types that cannot be used for\nprediction at this point\n")
#     } else {
#       if (sum(names(pred) %in% names(plug_data)) < length(names(pred))) {
#         return(cat("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n"))
#       } else {
#         plug_data[names(pred)] <- list(NULL)
#         pred <- data.frame(plug_data[-1],pred)
#       }
#     }
#   } else {
#     ## generate predictions for all observations in the dataset
#     pred <- getdata(pred_data, filt = "", na.rm = FALSE)
#     pred_names <- names(pred)
#     pred <- try(select_(pred, .dots = vars), silent = TRUE)
#     if (is(pred, 'try-error')) {
#       cat("Model variables: ")
#       cat(vars,"\n")
#       cat("Profile variables to be added: ")
#       cat(vars[!vars %in% pred_names])
#       return()
#     }
#     pred %<>% na.omit()
#     pred_type <- "data"
#   }

#   pred_val <- try(predict(object$model, pred, type = 'response', se.fit = TRUE), silent = TRUE)
#   if (!is(pred_val, 'try-error')) {
#     pred_val %<>% data.frame %>% select(1:2)
#     colnames(pred_val) <- c("Prediction","std.error")
#     pred <- data.frame(pred, pred_val, check.names = FALSE)

#     if (prn) {
#       cat("Generalized linear model (GLM)")
#       cat("\nLink function:", object$link)
#       cat("\nData         :", object$dataset)
#       if (object$data_filter %>% gsub("\\s","",.) != "")
#         cat("\nFilter       :", gsub("\\n","", object$data_filter))
#       cat("\nResponse variable    :", object$rvar)
#       cat("\nLevel                :", object$lev, "in", object$rvar)
#       cat("\nExplanatory variables:", paste0(object$evar, collapse=", "),"\n\n")

#       if (pred_type == "cmd") {
#         cat("Predicted values for:\n")
#       } else {
#         cat(paste0("Predicted values for profiles from dataset: ",pred_data,"\n"))
#       }

#       isNum <- c("Prediction", "std.error")
#       # pred %>% {.[, isNum] <- round(.[, isNum],dec); .} %>%
#       pred %>% dfprint(dec) %>% print(row.names = FALSE)
#     }

#     return(pred %>% set_class(c("ann_predict",class(.))))
#   } else {
#     paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred_val,"condition")$message, "\n\nPlease try again. Examples are shown in the help file.") %>% cat
#   }

#   return(invisible())
# }

# #' Plot method for the predict.ann function
# #'
# #' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
# #'
# #' @param x Return value from \code{\link{predict.ann}}.
# #' @param xvar Variable to display along the X-axis of the plot
# #' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
# #' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
# #' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different colour
# #' @param conf_lev Confidence level to use for prediction intervals (.95 is the default). Note that the error bars for predicitions are approximations at this point.
# #' @param ... further arguments passed to or from other methods
# #'
# #' @examples
# #' result <- ann("titanic", "survived", c("pclass","sex","age"), lev = "Yes")
# #' pred <- predict(result, pred_cmd = "pclass = levels(pclass)")
# #' plot(pred, xvar = "pclass")
# #' pred <- predict(result, pred_cmd = "age = 0:100")
# #' plot(pred, xvar = "age")
# #' pred <- predict(result, pred_cmd = "pclass = levels(pclass), sex = levels(sex)")
# #' plot(pred, xvar = "pclass", color = "sex")
# #' pred <- predict(result, pred_cmd = "pclass = levels(pclass), age = seq(0,100,20)")
# #' plot(pred, xvar = "pclass", color = "age")
# #' plot(pred, xvar = "age", color = "pclass")
# #' pred <- predict(result, pred_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,20)")
# #' plot(pred, xvar = "age", color = "sex", facet_col = "pclass")
# #' plot(pred, xvar = "age", color = "pclass", facet_col = "sex")
# #' pred <- predict(result, pred_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,5)")
# #' plot(pred, xvar = "age", color = "sex", facet_col = "pclass")
# #' plot(pred, xvar = "age", color = "pclass", facet_col = "sex")
# #'
# #' @seealso \code{\link{ann}} to generate the result
# #' @seealso \code{\link{summary.ann}} to summarize results
# #' @seealso \code{\link{plot.ann}} to plot results
# #' @seealso \code{\link{predict.ann}} to generate predictions
# #'
# #' @export
# plot.ann_predict <- function(x,
#                              xvar = "",
#                              facet_row = ".",
#                              facet_col = ".",
#                              color = "none",
#                              conf_lev = .95,
#                              ...) {

#   if (is.null(xvar) || xvar == "") return(invisible())

#   object <- x; rm(x)
#   if (is.character(object)) return(object)

#   object$ymin <- object$Prediction - qnorm(.5 + conf_lev/2)*object$std.error
#   object$ymax <- object$Prediction + qnorm(.5 + conf_lev/2)*object$std.error

#   byvar <- NULL
#   if (color != "none") byvar <- color
#   if (facet_row != ".")
#     byvar <- if (is.null(byvar)) facet_row else unique(c(byvar, facet_row))

#   if (facet_col != ".")
#     byvar <- if (is.null(byvar)) facet_col else unique(c(byvar, facet_col))

#   tbv <- if (is.null(byvar)) xvar else c(xvar, byvar)
#   tmp <- object %>% group_by_(.dots = tbv) %>% select_(.dots = c("Prediction","ymin","ymax")) %>% summarise_each(funs(mean))
#   if (color == 'none') {
#     p <- ggplot(tmp, aes_string(x=xvar, y="Prediction")) + geom_line(aes(group = 1))
#   } else {
#     p <- ggplot(tmp, aes_string(x=xvar, y="Prediction", color = color, group = color)) + geom_line()
#   }

#   if (facet_row != "." || facet_col != ".") {
#     facets <- if (facet_row == ".")  paste("~", facet_col)
#               else paste(facet_row, '~', facet_col)
#     facet_fun <- if (facet_row == ".") facet_wrap else facet_grid
#     p <- p + facet_fun(as.formula(facets))
#   }

#   if (is.factor(tmp[[xvar]]) || length(unique(tmp[[xvar]])) < 10)
#     p <- p + geom_pointrange(aes_string(ymin = "ymin", ymax = "ymax"), size=.3)
#   else
#     p <- p + geom_smooth(aes_string(ymin = "ymin", ymax = "ymax"), stat="identity")

#   sshhr( p )
# }

#' Store predicted values generated in the ann function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/ann.html} for an example in Radiant
#'
#' @param pred Return value from \code{\link{predict.ann}}
#' @param data Dataset name
#' @param name Variable name assigned to the predicted values
#'
#' @export
store_ann <- function(pred, data,
                      name = "predict_ann") {

  # if (!is.null(object$data_filter) && object$data_filter != "")
  # if (!is_empty(object$data_filter))
  #   return(message("Please deactivate data filters before trying to store predictions or residuals"))

  ## fix empty name input
  if (gsub("\\s","",name) == "") name <- paste0(type, "_ann")

  # if (type == "residuals") {
  #   store <- object$model$residuals
  # } else {
    ## gsub needed because trailing/leading spaces may be added to the variable name
    # name <- unlist(strsplit(name, ",")) %>% gsub("\\s","",.)
    # if (length(name) > 1) {
    #   name <- name[1:min(2, length(name))]
    #   ind <- which(colnames(object) == "Prediction") %>% {.:(. + length(name[-1]))}
    #   store <- object[,ind]
    # } else {
      # store <- object$Prediction
    # }
  # }

  changedata(data, vars = pred, var_names = name)
}
