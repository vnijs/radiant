#' Linear regression using OLS
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param dep_var The response variable in the regression
#' @param indep_var Explanatory variables in the regression
#' @param int_var Interaction terms to include in the model
#' @param check "standardize" to see standardized coefficient estimates. "stepwise" to apply step-wise selection of variables in estimation
#' @param dec Number of decimals to show
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in regression as an object of class regression
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' result <- regression("diamonds", "price", c("carat","clarity"), check = "standardize")
#'
#' @seealso \code{\link{summary.regression}} to summarize results
#' @seealso \code{\link{plot.regression}} to plot results
#' @seealso \code{\link{predict.regression}} to generate predictions
#'
#' @export
regression <- function(dataset, dep_var, indep_var,
                       int_var = "",
                       check = "",
                       dec = 3,
                       data_filter = "") {

  dat <- getdata(dataset, c(dep_var, indep_var), filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  if (any(summarise_each(dat, funs(does_vary)) == FALSE))
    return("One or more selected variables show no variation. Please select other variables." %>%
           set_class(c("regression",class(.))))

  vars <- ""
  var_check(indep_var, colnames(dat)[-1], int_var) %>%
    { vars <<- .$vars; indep_var <<- .$iv; int_var <<- .$intv }

  if ("standardize" %in% check) {
    isNum <- sapply(dat, is.numeric)
    if (sum(isNum > 0)) dat[,isNum] %<>% data.frame %>% mutate_each(funs(scale))
  }

  formula <- paste(dep_var, "~", paste(vars, collapse = " + ")) %>% as.formula

  if ("stepwise" %in% check) {
    # use k = 2 for AIC, use k = log(nrow(dat)) for BIC
    model <- lm(paste(dep_var, "~ 1") %>% as.formula, data = dat) %>%
      step(., k = 2, scope = list(upper = formula), direction = 'both')
  } else {
    model <- lm(formula, data = dat)
  }

  reg_coeff <- tidy(model)
  reg_coeff$` ` <- sig_stars(reg_coeff$p.value)
  reg_coeff[,c(2:5)] %<>% round(dec)

  ## print -0 when needed
  if (!"standardize" %in% check) {
    cz <- reg_coeff[[2]] == 0
    if (length(cz) > 0 && sum(cz) > 0) {
      tz <- reg_coeff[[4]] < 0
      ## added to 0.000 isn't rounded to 0
      reg_coeff[[2]][cz] <- paste0("0.",paste0(rep(0,dec),collapse = ""))
      ## print -0 when needed
      reg_coeff[[2]][cz & tz] <- paste0("-0.",paste0(rep(0,dec),collapse = ""))
    }
  }

  reg_coeff$p.value[reg_coeff$p.value < .001] <- "< .001"
  colnames(reg_coeff) <- c("  ","coefficient","std.error","t.value","p.value"," ")

  isFct <- sapply(select(dat,-1), is.factor)
  if (sum(isFct) > 0) {
    for (i in names(select(dat,-1)[isFct]))
      reg_coeff$`  ` %<>% sub(i, paste0(i," > "), .)

    rm(i, isFct)
  }

  ## dat is not needed elsewhere and is already in "model" anyway
  rm(dat)

  environment() %>% as.list %>% set_class(c("regression",class(.)))
}

#' Summary method for the regression function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regression}}
#' @param sum_check Optional output or estimation parameters. "rsme" to show the root mean squared error. "sumsquares" to show the sum of squares table. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates.
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param test_var Variables to evaluate in model comparison (i.e., a competing models F-test)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' summary(result, sum_check = c("rmse","sumsquares","vif","confint"), test_var = "clarity")
#' result <- regression("shopping", "v1", c("v2","v3"))
#' summary(result, test_var = "v2")
#' shopping %>% regression("v1", "v2:v6") %>% summary
#'
#' @seealso \code{\link{regression}} to generate the results
#' @seealso \code{\link{plot.regression}} to plot results
#' @seealso \code{\link{predict.regression}} to generate predictions
#'
#' @importFrom car vif
#'
#' @export
summary.regression <- function(object,
                               sum_check = "",
                               conf_lev = .95,
                               test_var = "",
                               ...) {


  if (is.character(object)) return(object)
  if (class(object$model)[1] != 'lm') return(object)

  dec <- object$dec

  if ("stepwise" %in% object$check) cat("\n-----------------------------------------------\n\n")

  # cat("Time",now(),"\n")
  cat("Linear regression (OLS)\n")
  cat("Data     :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
  cat("Response variable    :", object$dep_var, "\n")
  cat("Explanatory variables:", paste0(object$indep_var, collapse=", "), "\n")
  expl_var <- if (length(object$indep_var) == 1) object$indep_var else "x"
  cat(paste0("Null hyp.: the effect of ", expl_var, " on ", object$dep_var, " is zero\n"))
  cat(paste0("Alt. hyp.: the effect of ", expl_var, " on ", object$dep_var, " is not zero\n"))
  if ("standardize" %in% object$check)
    cat("Standardized coefficients shown\n")

  cat("\n")
  if (all(object$reg_coeff$p.value == "NaN")) {
    print(object$reg_coeff[,1:2], row.names=FALSE)
    cat("\nInsufficient variation in explanatory variable(s) to report additional statistics")
    return()
  } else {
    print(object$reg_coeff, row.names=FALSE)
  }

  if (nrow(object$model$model) <= (length(object$indep_var) + 1))
    return("\nInsufficient observations to estimate model")

  ## adjusting df for included intercept term
  df_int <- if (attr(object$model$terms, "intercept")) 1L else 0L

  reg_fit <- glance(object$model) %>% round(dec)
  if (reg_fit['p.value'] < .001) reg_fit['p.value'] <- "< .001"
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("R-squared:", paste0(reg_fit$r.squared, ", "), "Adjusted R-squared:", reg_fit$adj.r.squared, "\n")
  cat("F-statistic:", reg_fit$statistic, paste0("df(", reg_fit$df - df_int, ",", reg_fit$df.residual, "), p.value"), reg_fit$p.value)
  cat(paste0("\nNr obs: ", reg_fit$df + reg_fit$df.residual))
  cat("\n\n")

  if ("rmse" %in% sum_check) {
    mean(object$model$residuals^2, na.rm=TRUE) %>% sqrt %>% round(dec) %>%
    cat("Prediction error (RMSE): ", ., "\n\n")
  }

  if ("sumsquares" %in% sum_check) {
    atab <- anova(object$model)
    nr_rows <- dim(atab)[1]
    df_reg <- sum(atab$Df[-nr_rows])
    df_err <- sum(atab$Df[nr_rows])
    df_tot <- df_reg + df_err

    ss_reg <- sum(atab$`Sum Sq`[-nr_rows])
    ss_err <- sum(atab$`Sum Sq`[nr_rows])
    ss_tot <- ss_reg + ss_err
    ss_tab <- data.frame(matrix(nrow = 3, ncol = 2))
    rownames(ss_tab) <- c("Regression","Error","Total")
    colnames(ss_tab) <- c("df","SS")
    ss_tab$df <- c(df_reg,df_err,df_tot)
    ss_tab$SS <- c(ss_reg,ss_err,ss_tot)
    cat("Sum of squares:\n")
    format(ss_tab, scientific = FALSE) %>% print
    cat("\n")
  }

  if ("vif" %in% sum_check) {
    if (anyNA(object$model$coeff)) {
      cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\nmulticollinearity diagnostics were not calculated.\n")
    } else {
      if (length(object$indep_var) > 1) {
        cat("Variance Inflation Factors\n")
        vif (object$model) %>%
          { if (!dim(.) %>% is.null) .[,"GVIF"] else . } %>% ## needed when factors are included
          data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
          round(2) %>%
          .[order(.$VIF, decreasing=T),] %>%
          { if (nrow(.) < 8) t(.) else . } %>%
          print
      } else {
        cat("Insufficient explanatory variables to calculate\nmulticollinearity diagnostics (VIF)")
      }
    }
    cat("\n")
  }

  if ("confint" %in% sum_check) {
    if (anyNA(object$model$coeff)) {
      cat("There is perfect multicollineary in the set of explanatory variables.\nOne or more variables were dropped from the estimation. Confidence\nintervals were not calculated.\n")
    } else {

      cl_split <- function(x) 100*(1-x)/2
      cl_split(conf_lev) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
      (100 - cl_split(conf_lev)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

      confint(object$model, level = conf_lev) %>%
        as.data.frame %>%
        set_colnames(c("Low","High")) %>%
        { .$`+/-` <- (.$High - .$Low)/2; . } %>%
        round(dec) %>%
        cbind(object$reg_coeff[[2]],.) %>%
        set_rownames(object$reg_coeff$`  `) %>%
        set_colnames(c("coefficient", cl_low, cl_high, "+/-")) %>%
        print
      cat("\n")
    }
  }

  if (!is.null(test_var) && test_var[1] != "") {
    if ("stepwise" %in% object$check) {
      cat("Model comparisons are not conducted when Stepwise has been selected.\n")
    } else {
      sub_formula <- ". ~ 1"

      vars <- object$indep_var
      if (object$int_var != "" && length(vars) > 1) {
        ## updating test_var if needed
        test_var <- test_specs(test_var, object$int_var)
        vars <- c(vars,object$int_var)
      }

      not_selected <- setdiff(vars,test_var)
      if (length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))
      sub_mod <- update(object$model, sub_formula, data = object$model$model) %>%
                   anova(object$model, test='F')

      if (sub_mod[,"Pr(>F)"][2] %>% is.na) return(cat(""))
      p.value <- sub_mod[,"Pr(>F)"][2] %>% { if (. < .001) "< .001" else round(.,dec) }

      cat(attr(sub_mod,"heading")[2])
        object$model$model[,1] %>%
        { sum((. - mean(.))^2) } %>%
        {1 - (sub_mod$RSS / .)} %>%
        round(dec) %>%
        cat("\nR-squared, Model 1 vs 2:", .)
      cat("\nF-statistic:", sub_mod$F[2] %>% round(dec), paste0("df(", sub_mod$Res.Df[1]-sub_mod$Res.Df[2], ",", sub_mod$Res.Df[2], "), p.value ", p.value))
    }
  }
}

#' Plot method for the regression function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{regression}}
#' @param plots Regression plots to produce for the specified regression model. Enter "" to avoid showing any plots (default). "hist" to show histograms of all variables in the model. "correlations" for a visual representation of the correlation matrix selected variables. "scatter" to show scatter plots (or box plots for factors) for the response variable with each explanatory variable. "dashboard" for a series of six plots that can be used to evaluate model fit visually. "resid_pred" to plot the explanatory variables against the model residuals. "coef" for a coefficient plot with adjustable confidence intervals. "leverage" to show leverage plots for each explanatory variable
#' @param lines Optional lines to include in the select plot. "line" to include a line through a scatter plot. "loess" to include a polynomial regression fit line. To include both use c("line","loess")
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param intercept Include the intercept in the coefficient plot (TRUE, FALSE). FALSE is the default
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' plot(result, plots = "dashboard")
#' plot(result, plots = "dashboard", lines = c("line","loess"))
#' plot(result, plots = "coef", intercept = TRUE)
#' plot(result, plots = "coef", conf_lev = .99, intercept = TRUE)
#' plot(result, plots = "hist")
#' plot(result, plots = "scatter", lines = c("line","loess"))
#' plot(result, plots = "correlations")
#' plot(result, plots = "leverage")
#' plot(result, plots = "resid_pred", lines = "line")
#'
#' @seealso \code{\link{regression}} to generate the results
#' @seealso \code{\link{summary.regression}} to summarize results
#' @seealso \code{\link{predict.regression}} to generate predictions
#'
#' @importFrom car leveragePlots
#'
#' @export
plot.regression <- function(x,
                            plots = "",
                            lines = "",
                            conf_lev = .95,
                            intercept = FALSE,
                            shiny = FALSE,
                            ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)

  dec <- object$dec

  if (class(object$model)[1] != 'lm') return(object)

  if (plots[1] == "")
    return(cat("Please select a regression plot from the drop-down menu"))

  # no plots if aliased coefficients present
  if (anyNA(object$model$coeff)) plots <- return("")

  # object_size(object$model, model)
  model <- ggplot2::fortify(object$model)

  dep_var <- object$dep_var
  indep_var <- object$indep_var
  vars <- c(dep_var, indep_var)

  flines <- sub("loess","",lines) %>% sub("line","",.)
  nlines <- sub("jitter","",lines)

  plot_list <- list()
  if ("hist" %in% plots)
    for (i in vars) {
      plot_list[[paste0("hist",i)]] <-
        visualize(select_(model, .dots = i), xvar = i, bins = 10, custom = TRUE)
        # ggplot(model[,vars], aes_string(x = i)) + geom_histogram(alpha = 0.5)
    }

  if ("dashboard" %in% plots) {

    plot_list[[1]] <-
      visualize(model, xvar = ".fitted", yvar = dep_var, type = "scatter", custom = TRUE) +
      labs(list(title = "Actual vs Fitted values", x = "Fitted", y = "Actual"))

    plot_list[[2]] <-
      visualize(model, xvar = ".fitted", yvar = ".resid", type = "scatter", custom = TRUE) +
      labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))

    plot_list[[3]] <- ggplot(model, aes(y=.resid, x=seq_along(.resid))) + geom_line() +
      labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))

    plot_list[[4]] <- ggplot(model, aes_string(sample=".stdresid")) + stat_qq(alpha = .5) +
      labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))

    plot_list[[5]] <-
      visualize(model, xvar = ".resid", custom = TRUE) +
      labs(list(title = "Histogram of residuals", x = "Residuals"))

    plot_list[[6]] <- ggplot(model, aes_string(x=".resid")) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(model[,'.resid']), sd = sd(model[,'.resid'])), color = "blue") +
      labs(list(title = "Residual vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())

    if ("loess" %in% lines)
      for (i in 1:3) plot_list[[i]] <- plot_list[[i]] + sshhr( geom_smooth(method = "loess", size = .75, linetype = "dotdash") )

    if ("line" %in% lines) {
      for (i in c(1,4))
        plot_list[[i]] <- plot_list[[i]] + geom_abline(linetype = 'dotdash')
      for (i in 2:3)
        plot_list[[i]] <- plot_list[[i]] + sshhr( geom_smooth(method = "lm", se = FALSE, size = .75, linetype = "dotdash", colour = 'black') )
    }
  }

  if ("scatter" %in% plots) {
    for (i in indep_var) {
      if ('factor' %in% class(model[,i])) {
        plot_list[[paste0("scatter",i)]] <-
          visualize(select_(model, .dots = c(i,dep_var)), xvar = i, yvar = dep_var, type = "scatter", check = flines, alpha = .2, custom = TRUE)
      } else {
        plot_list[[paste0("scatter",i)]] <-
          visualize(select_(model, .dots = c(i,dep_var)), xvar = i, yvar = dep_var, type = "scatter", check = nlines, custom = TRUE)
      }
    }
  }

  if ("resid_pred" %in% plots) {
    for (i in indep_var) {
      if ('factor' %in% class(model[,i])) {
        plot_list[[i]] <-
          visualize(select_(model, .dots = c(i,".resid")), xvar = i, yvar = ".resid", type = "scatter", check = flines, alpha = .2, custom = TRUE) +
          ylab("residuals")
      } else {
        plot_list[[i]] <-
          visualize(select_(model, .dots = c(i,".resid")), xvar = i, yvar = ".resid", type = "scatter", check = nlines, custom = TRUE) +
          ylab("residuals")
      }
    }
  }

  if ("coef" %in% plots) {
    if (!anyNA(object$model$coeff)) {
      cl_split <- function(x) 100*(1-x)/2
      cl_split(conf_lev) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
      (100 - cl_split(conf_lev)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

      confint(object$model, level = conf_lev) %>%
        data.frame %>%
        set_colnames(c("Low","High")) %>%
        cbind(select(object$reg_coeff,2),.) %>%
        round(dec) %>%
        set_rownames(object$reg_coeff$`  `) %>%
        { if (!intercept) .[-1,] else . } %>%
        mutate(variable = rownames(.)) %>%
          ggplot() +
            geom_pointrange(aes_string(x = "variable", y = "coefficient",
                            ymin = "Low", ymax = "High")) +
            geom_hline(yintercept = 0, linetype = 'dotdash', color = "blue") + coord_flip() -> p
            return(p)
    }
  }

  if ("correlations" %in% plots)
    return(plot.correlation_(object$model$model))

  if ("leverage" %in% plots)
    return(leveragePlots(object$model, main = "", ask=FALSE, id.n = 1,
           layout = c(ceiling(length(indep_var)/2),2)))

  if (exists("plot_list")) {
    sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 2))) ) %>%
        {if (shiny) . else print(.)}
  }
}

#' Predict method for the regression function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regression}}
#' @param pred_vars Variables to use for prediction
#' @param pred_data Name of the dataset to use for prediction
#' @param pred_cmd Command used to generate data for prediction
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param prn Print prediction results (default is TRUE)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' predict(result, pred_cmd = "carat = 1:10")
#' predict(result, pred_cmd = "clarity = levels(clarity)")
#' result <- regression("diamonds", "price", c("carat","clarity"), int_var = c("carat:clarity"))
#' dpred <<- getdata("diamonds") %>% slice(1:10)
#' predict(result, pred_data = "dpred")
#' rm(dpred, envir = .GlobalEnv)
#'
#' @seealso \code{\link{regression}} to generate the result
#' @seealso \code{\link{summary.regression}} to summarize results
#' @seealso \code{\link{plot.regression}} to plot results
#'
#' @export
predict.regression <- function(object,
                               pred_vars = "",
                               pred_data = "",
                               pred_cmd = "",
                               conf_lev = 0.95,
                               prn = TRUE,
                               ...) {

  if (is.character(object)) return(object)

  # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression as starting point
  pred_count <- sum(c(pred_vars == "", pred_cmd == "", pred_data == ""))
  if ("standardize" %in% object$check) {
    return(cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the standardized coefficients box and try again"))
  } else if (pred_count == 3) {
    return(cat("Please select variables, data, or specify a command to generate predictions. For example,\ncarat = seq(.5, 1.5, .1) would produce predictions for values of\ncarat starting at .5, increasing to 1.5 in increments of .1. Make\nsure to press return after you finish entering the command.\nIf no results are shown the command was likely invalid. Alternatively,\nspecify a dataset to generate predictions. You could create this in\nExcel and use the paste feature in Data > Manage to bring it into\nRadiant"))
  }

  if (pred_count < 2) {
    if (pred_cmd != "")
      cat("Multiple inputs where specified for prediciton. The command will be used.\nTo use variables or a dataset remove the command.")
    if (pred_vars != "")
      cat("Multiple inputs where specified for prediciton. The variables selected will be used.\nTo use a command or dataset unselect variables.")
  }

  pred_type <- "cmd"
  vars <- object$indep_var
  if (pred_cmd != "") {
    pred_cmd %<>% gsub("\"","\'", .) %>% gsub(";",",", .)
    pred <- try(eval(parse(text = paste0("with(object$model$model, expand.grid(", pred_cmd ,"))"))), silent = TRUE)
    if (is(pred, 'try-error')) {
      paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred,"condition")$message, "\n\nPlease try again. Examples are shown in the help file.") %>% cat
      return()
    }

    # adding information to the prediction data.frame
    dat_classes <- attr(object$model$term, "dataClasses")[-1]
    isFct <- dat_classes == "factor"
    isNum <- dat_classes == "numeric"
    dat <- select_(object$model$model, .dots = vars)

    # based on http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
    max_freq <- function(x) names(which.max(table(x)))

    plug_data <- data.frame(init___ = 1)
    if (sum(isNum) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(mean), vars[isNum]))
    if (sum(isFct) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(max_freq), vars[isFct]))

     rm(dat)

    if (sum(isNum) + sum(isFct) < length(vars)) {
      cat("The model includes data-types that cannot be used for\nprediction at this point\n")
    } else {
      if (sum(names(pred) %in% names(plug_data)) < length(names(pred))) {
        return(cat("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n"))
      } else {
        plug_data[names(pred)] <- list(NULL)
        pred <- data.frame(plug_data[,-1],pred)
      }
    }
  } else {
    ## generate predictions for all observations in the dataset
    pred <- getdata(pred_data, filt = "", na.rm = FALSE)
    pred_names <- names(pred)
    pred <- try(select_(pred, .dots = vars), silent = TRUE)
    if (is(pred, 'try-error')) {
      cat("Model variables: ")
      cat(vars,"\n")
      cat("Profile variables to be added: ")
      cat(vars[!vars %in% pred_names])
      return()
    }
    pred %<>% na.omit()
    pred_type <- "data"
  }

  pred_val <- try(predict(object$model, pred, interval = 'prediction', level = conf_lev), silent = TRUE)
  if (!is(pred_val, 'try-error')) {
    pred_val %<>% data.frame %>% mutate(diff = .[,3] - .[,1])
    cl_split <- function(x) 100*(1-x)/2
    cl_split(conf_lev) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
    (100 - cl_split(conf_lev)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high
    colnames(pred_val) <- c("Prediction",cl_low,cl_high,"+/-")

    pred <- data.frame(pred, pred_val, check.names = FALSE)

    if (prn) {
      cat("Linear regression (OLS)\n")
      cat("Data     :", object$dataset, "\n")
      if (object$data_filter %>% gsub("\\s","",.) != "")
        cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
      cat("Response variable    :", object$dep_var, "\n")
      cat("Explanatory variables:", paste0(object$indep_var, collapse=", "), "\n\n")

      if (pred_type == "cmd") {
        cat("Predicted values for:\n")
      } else {
        cat(paste0("Predicted values for profiles from dataset: ",object$pred_data,"\n"))
      }

      pred %>% print(., row.names = FALSE)
    }

    # pushing predictions into the clipboard
    # os_type <- Sys.info()["sysname"]
    # if (os_type == 'Windows') {
    #   write.table(pred, "clipboard", sep="\t", row.names=FALSE)
    # } else if (os_type == "Darwin") {
    #   write.table(pred, file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
    # }
    # if (os_type != "Linux")
    #   cat("\nPredictions were pushed to the clipboard. You can paste them in Excel or\nuse Manage > Data to paste the predictions as a new dataset.\n\n")

    return(pred %>% set_class(c("reg_predict",class(.))))

  } else {
    paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred_val,"condition")$message, "\n\nPlease try again. Examples are shown in the help file.") %>% cat
  }
}

#' Plot method for the predict.regression function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{predict.regression}}.
#' @param xvar Variable to display along the X-axis of the plot
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different colour
#' @param conf_lev Confidence level to use for prediction intervals (.95 is the default). Note that the error bars for predicitions are approximations at this point.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' pred <- predict(result, pred_cmd = "carat = 1:10")
#' plot(pred, xvar = "carat")
#' result <- regression("diamonds", "price", c("carat","clarity"), int_var = "carat:clarity")
#' dpred <<- getdata("diamonds") %>% slice(1:100)
#' pred <- predict(result, pred_data = "dpred")
#' plot(pred, xvar = "carat", color = "clarity")
#' rm(dpred, envir = .GlobalEnv)
#'
#' @seealso \code{\link{regression}} to generate the result
#' @seealso \code{\link{summary.regression}} to summarize results
#' @seealso \code{\link{plot.regression}} to plot results
#' @seealso \code{\link{predict.regression}} to generate predictions
#'
#' @export
plot.reg_predict <- function(x,
                             xvar = "",
                             facet_row = ".",
                             facet_col = ".",
                             color = "none",
                             conf_lev = .95,
                             ...) {

  if (is.null(xvar) || xvar == "") return(invisible())

  object <- x; rm(x)
  if (is.character(object)) return(object)

  cn <- colnames(object)
  cn[which(cn == "Prediction") + 1] <- "ymin"
  cn[which(cn == "Prediction") + 2] <- "ymax"
  colnames(object) <- cn

  # if (facet_row != ".") {
  #   byvar <- facet_row
  # }
  # if (facet_col != ".") {
  #   byvar <- if (is.null(byvar)) facet_col else c(byvar, facet_col)
  # }
  # if (fill != "none") {
  #   vars %<>% c(., fill)
  #   if (type == "bar")
  #     byvar <- if (is.null(byvar)) fill else c(byvar, fill)
  # }

  # tbv <- if (is.null(byvar)) i else c(i, byvar)
  # tmp <- dat %>% group_by_(.dots = tbv) %>% select_(j) %>% summarise_each(funs(mean))
  # print(getclass(object))


  if (color == "none") {
    p <- ggplot(object, aes_string(x=xvar, y="Prediction")) +
           geom_line()
           # geom_line(aes(group=1))
  } else {
    p <- ggplot(object, aes_string(x = xvar, y = "Prediction", color = color, group = color)) +
           geom_line()
                # geom_line(aes_string(group=color))
  }

  facets <- paste(facet_row, "~", facet_col)
  if (facets != ". ~ .") p <- p + facet_grid(facets)

  if (length(unique(object[[xvar]])) < 10)
    p <- p + geom_pointrange(aes_string(ymin = "ymin", ymax = "ymax"), size=.3)
  else
    p <- p + geom_smooth(aes_string(ymin = "ymin", ymax = "ymax"), stat="identity")

  sshhr( p )
}

#' Store residuals or predicted values generated in the regression function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regression}} or \code{\link{predict.regression}}
#' @param data Dataset name
#' @param type Residuals ("residuals") or predictions ("predictions"). For predictions the dataset name must be provided
#' @param name Variable name assigned to the residuals or predicted values
#'
#' @examples
#' \donttest{
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' store_reg(result)
#' }
#' @export
store_reg <- function(object, data = object$dataset,
                      type = "residuals", name = paste0(type, "_reg")) {
  if (!is.null(object$data_filter) && object$data_filter != "")
    return(message("Please deactivate data filters before trying to store predictions or residuals"))

  ## fix empty name input
  if (gsub("\\s","",name) == "") name <- paste0(type, "_reg")

  if (type == "residuals") {
    store <- object$model$residuals
  } else {
    ## gsub needed because trailing/leading spaces may be added to the variable name
    name <- unlist(strsplit(name, ",")) %>% gsub("\\s","",.)
    if (length(name) > 1) {
      name <- name[1:min(3, length(name))]
      ind <- which(colnames(object) == "Prediction") %>% {.:(. + length(name[-1]))}
      store <- object[,ind]
    } else {
      store <- object$Prediction
    }
  }

  changedata(data, vars = store, var_names = name)
}

#' Check if main effects for all interaction effects are included in the model
#' If ':' is used to select a range _indep_var_ is updated
#' @details See \url{http://vnijs.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param iv List of explanatory variables provided to _regression_ or _glm_
#' @param cn Column names for all explanatory variables in _dat_
#' @param intv Interaction terms specified
#'
#' @return 'vars' is a vector of right-hand side variables, possibly with interactions, 'iv' is the list of explanatory variables, and intv are interaction terms
#'
#' @examples
#' var_check("a:d", c("a","b","c","d"))
#' var_check(c("a", "b"), c("a", "b"), "a:c")
#'
#' @export
var_check <- function(iv, cn, intv = "") {

  ## if : is used to select a range of variables indep_var is updated
  vars <- iv
  if (length(vars) < length(cn)) vars <- iv <- cn

  if (intv != "" && length(vars) > 1) {
    if ({intv %>% strsplit(":") %>% unlist} %in% vars %>% all) {
      vars <- c(vars, intv)
    } else{
      cat("Interaction terms contain variables not selected as main effects.\nRemoving all interactions from the estimation")
      intv <- ""
    }
  }

  list(vars = vars, iv = iv, intv = intv)
}

#' Add interaction terms to list of test variables if needed
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param test_var List of variables to use for testing for regression or glm_reg
#' @param int_var Interaction terms specified
#'
#' @return A vector of variables names to test
#'
#' @examples
#' test_specs("a", c("a:b", "b:c"))
#'
#' @export
test_specs <- function(test_var, int_var) {

  if ({int_var %>% strsplit(":") %>% unlist} %in% test_var %>% any) {
    cat("Interaction terms contain variables specified for testing.\nRelevant interaction terms are included in the requested test.\n\n")
    for (i in test_var) test_var <- c(test_var, int_var[grep(i, int_var)])
    test_var <- unique(test_var)
  }
  test_var
}
