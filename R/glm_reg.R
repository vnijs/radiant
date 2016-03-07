#' Generalized linear models (GLM)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable in the logit (probit) model
#' @param evar Explanatory variables in the model
#' @param lev The level in the response variable defined as _success_
#' @param link Link function for glm ('logit' or 'probit'). 'logit' is the default
#' @param int Interaction term to include in the model
#' @param wts Weights to use in estimation
#' @param check Optional estimation parameters. "standardize" to output standardized coefficient estimates. "stepwise" to apply step-wise selection of variables
#' @param dec Number of decimals to show
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in glm_reg as an object of class glm_reg
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' result <- glm_reg("titanic", "survived", c("pclass","sex"))
#'
#' @seealso \code{\link{summary.glm_reg}} to summarize the results
#' @seealso \code{\link{plot.glm_reg}} to plot the results
#' @seealso \code{\link{predict.glm_reg}} to generate predictions
#' @seealso \code{\link{plot.glm_predict}} to plot prediction output
#'
#' @importFrom sandwich vcovHC
#'
#' @export
glm_reg <- function(dataset, rvar, evar,
                    lev = "",
                    link = "logit",
                    int = "",
                    wts = "None",
                    check = "",
                    dec = 3,
                    data_filter = "") {

  if (rvar %in% evar)
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
           set_class(c("glm_reg",class(.))))

  vars <- c(rvar, evar)

  if (!is.null(wts) && wts == "None") {
    wts <- NULL
  } else {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  # dat <- getdata(dataset, c(rvar, evar), filt = data_filter)
  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  if (!is.null(wts)) {
    wts <- dat[[wtsname]]
    if (!is.integer(wts)) {
      wts_int <- as.integer(wts)
      if (all(wts == wts_int)) wts <- wts_int
      rm(wts_int)
    }
    dat <- select_(dat, .dots = paste0("-",wtsname))
  }

  if (any(summarise_each(dat, funs(does_vary)) == FALSE))
    return("One or more selected variables show no variation. Please select other variables." %>%
           set_class(c("glm_reg",class(.))))

  rv <- dat[[rvar]]
  if (lev == "") {
    if (is.factor(rv))
      lev <- levels(rv)[1]
    else
      lev <- as.character(rv) %>% as.factor %>% levels %>% .[1]
  }

  ## transformation to TRUE/FALSE depending on the selected level (lev)
  dat[[rvar]] <- dat[[rvar]] == lev

  vars <- ""
  var_check(evar, colnames(dat)[-1], int) %>%
    { vars <<- .$vars; evar <<- .$ev; int <<- .$intv }

  if ("standardize" %in% check) {
    ## express evars in std. deviations
    isNum <- sapply(dat, is.numeric)
    if (sum(isNum) > 0) {
      if (length(wts) > 0) {
        ## use weighted.sd if weights are used in estimation
        dat[,isNum] %<>% data.frame %>% mutate_each(funs(. / (2*weighted.sd(., wts, na.rm = TRUE))))
      } else {
        dat[,isNum] %<>% data.frame %>% mutate_each(funs(. / (2*sd(., na.rm = TRUE))))
      }
    }
  }

  form <- paste(rvar, "~", paste(vars, collapse = " + ")) %>% as.formula

  if ("stepwise" %in% check) {
    ## use k = 2 for AIC, use k = log(nrow(dat)) for BIC
    model <- sshhr(glm(as.formula(paste(rvar, "~ 1")), weights = wts,
                 family = binomial(link = link), data = dat) %>%
             step(k = 2, scope = list(upper = form), direction = 'both'))
  } else {
    model <- sshhr(glm(form, weights = wts, family = binomial(link = link), data = dat))
  }

  coeff <- tidy(model)
  coeff$` ` <- sig_stars(coeff$p.value) %>% format(justify = "left")
  colnames(coeff) <- c("  ","coefficient","std.error","z.value","p.value"," ")

  isFct <- sapply(select(dat,-1), function(x) is.factor(x) || is.logical(x))
  if (sum(isFct) > 0) {
    for (i in names(isFct[isFct]))
      coeff$`  ` %<>% gsub(i, paste0(i,"|"), .) %>% gsub("\\|\\|","\\|",.)

    rm(i, isFct)
  }
  coeff$`  ` %<>% format(justify = "left")

  if (!is_empty(wts, "None") && class(wts) != "integer") {
    vcov <- sandwich::vcovHC(model, type="HC0")
    # coeff$std.error <- sqrt(diag(sandwich::vcovHC(model, type="HC0")))
    coeff$std.error <- sqrt(diag(vcov))
    coeff$z.value <- coef(model) / coeff$std.error
    coeff$p.value <- 2 * pnorm(abs(coef(model)/coeff$std.error), lower.tail=FALSE)
    coeff$` ` <- sig_stars(coeff$p.value) %>% format(justify = "left")
  }

  colnames(coeff) <- c("  ","coefficient","std.error","z.value","p.value"," ")
  if (link == "logit") {
    coeff$OR <- exp(coeff$coefficient)
  } else {
    coeff$OR <- ""
  }
  coeff <- coeff[,c("  ","OR", "coefficient","std.error","z.value","p.value"," ")]

  rm(dat) ## dat not needed elsewhere

  environment() %>% as.list %>% set_class(c("glm_reg",class(.)))
}

#' Summary method for the glm_reg function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{glm_reg}}
#' @param sum_check Optional output. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates. "odds" to show odds ratios and confidence interval estimates.
#' @param conf_lev Confidence level to use for coefficient and odds confidence intervals (.95 is the default)
#' @param test_var Variables to evaluate in model comparison (i.e., a competing models Chi-squared test)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- glm_reg("titanic", "survived", "pclass", lev = "Yes")
#' summary(result, test_var = "pclass")
#' res <- glm_reg("titanic", "survived", c("pclass","sex"), int="pclass:sex", lev="Yes")
#' summary(res, sum_check = c("vif","confint","odds"))
#' titanic %>% glm_reg("survived", c("pclass","sex","age"), lev = "Yes") %>% summary("vif")
#'
#' @seealso \code{\link{glm_reg}} to generate the results
#' @seealso \code{\link{plot.glm_reg}} to plot the results
#' @seealso \code{\link{predict.glm_reg}} to generate predictions
#' @seealso \code{\link{plot.glm_predict}} to plot prediction output
#'
#' @importFrom car vif linearHypothesis
#' @importFrom sandwich vcovHC
#'
#' @export
summary.glm_reg <- function(object,
                            sum_check = "",
                            conf_lev = .95,
                            test_var = "",
                            ...) {

  if (is.character(object)) return(object)
  if (class(object$model)[1] != 'glm') return(object)

  dec <- object$dec

  if ("stepwise" %in% object$check) cat("-----------------------------------------------\n")
  cat("Generalized linear model (GLM)")
  cat("\nLink function:", object$link)
  cat("\nData         :", object$dataset)
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter       :", gsub("\\n","", object$data_filter))
  cat("\nResponse variable    :", object$rvar)
  cat("\nLevel                :", object$lev, "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse=", "),"\n")
  if (length(object$wtsname) > 0)
    cat("Weights used         :", object$wtsname, "\n")
  expl_var <- if (length(object$evar) == 1) object$evar else "x"
  cat(paste0("Null hyp.: there is no effect of ", expl_var, " on ", object$rvar, "\n"))
  cat(paste0("Alt. hyp.: there is an effect of ", expl_var, " on ", object$rvar, "\n"))
  if ("standardize" %in% object$check) {
    if (object$link == "logit")
      cat("**Standardized odds-ratios and coefficients shown**\n")
    else
      cat("**Standardized coefficients shown**\n")
  }
  if (!is_empty(object$wts, "None") && class(object$wts) != "integer")
    cat("**Robust standard errors used**\n")
  cat("\n")

  coeff <- object$coeff
  p.small <- coeff$p.value < .001
  coeff[,2:6] %<>% dfprint(dec)
  coeff$p.value[p.small] <- "< .001"
  coeff %>% {.$OR[1] <- ""; .} %>% print(row.names=FALSE)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  glm_fit <- glance(object$model)

  ## pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_regression
  glm_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>% round(dec)
  if (!is_empty(object$wts, "None") && class(object$wts) == "integer") {
    nobs <- sum(object$wts)
    glm_fit$BIC <- round(-2*glm_fit$logLik + ln(nobs) * with(glm_fit, 1 + df.null - df.residual), dec)
  } else {
    nobs <- glm_fit$df.null + 1
  }

  ## chi-squared test of overall model fit (p-value) - http://www.ats.ucla.edu/stat/r/dae/logit.htm
  chi_pval <- with(object$model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
  chi_pval %<>% { if (. < .001) "< .001" else round(.,dec) }

  cat("\nPseudo R-squared:", glm_fit$r2)
  cat(paste0("\nLog-likelihood: ", glm_fit$logLik, ", AIC: ", glm_fit$AIC, ", BIC: ", glm_fit$BIC))
  cat(paste0("\nChi-squared: ", with(glm_fit, null.deviance - deviance) %>% round(dec), " df(",
               with(glm_fit, df.null - df.residual), "), p.value ", chi_pval), "\n")
  cat("Nr obs:", nrprint(nobs, dec = 0), "\n\n")

  if (anyNA(object$model$coeff))
    cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\n")

  if ("vif" %in% sum_check) {
    if (anyNA(object$model$coeff)) {
      # cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\nmulticollinearity diagnostics were not calculated.\n")
      cat("Multicollinearity diagnostics were not calculated.")
    } else {
      if (length(object$evar) > 1) {
        cat("Variance Inflation Factors\n")
        vif(object$model) %>%
          { if (!dim(.) %>% is.null) .[,"GVIF"] else . } %>% ## needed when factors are included
          data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
          round(dec) %>%
          .[order(.$VIF, decreasing=T),] %>%
          { if (nrow(.) < 8) t(.) else . } %>%
          print
      } else {
        cat("Insufficient number of explanatory variables selected to calculate\nmulticollinearity diagnostics")
      }
    }
    cat("\n")
  }

  if (any(c("confint","odds") %in% sum_check)) {
    if (any(is.na(object$model$coeff))) {
      cat("There is perfect multicollineary in the set of explanatory variables.\nOne or more variables were dropped from the estimation.\n")
      cat("Confidence intervals were not calculated.\n")
    } else {
      ci_perc <- ci_label(cl = conf_lev)

      # cnfint <- ifelse (is_empty(object$wts, "None"), confint.default, radiant::confint_robust)
      # if (is_empty(object$wts, "None"))
      if (!is_empty(object$wts, "None") && class(object$wts) != "integer")
        cnfint <- radiant::confint_robust
      else
        cnfint <- confint.default

      ci_tab <-
        # confint.default(object$model, level = conf_lev) %>%
        cnfint(object$model, level = conf_lev, vcov = object$vcov) %>%
        as.data.frame %>%
        set_colnames(c("Low","High")) %>%
        cbind(select(object$coeff,3),.)
        # cbind(object$coeff[["coefficient"]],.)
        # cbind(object$coeff$coefficient,.)

      if ("confint" %in% sum_check) {
        ci_tab %T>%
        { .$`+/-` <- (.$High - .$coefficient) } %>%
        dfprint(dec) %>%
        set_colnames(c("coefficient", ci_perc[1], ci_perc[2], "+/-")) %>%
        set_rownames(object$coeff$`  `) %>%
        print
        cat("\n")
      }
    }
  }

  if ("odds" %in% sum_check) {
    if (any(is.na(object$model$coeff))) {
      cat("Odds ratios were not calculated\n")
    } else {
      if (object$link == "logit") {
        orlab <- if ("standardize" %in% object$check) "std odds ratio" else "odds ratio"
        exp(ci_tab[-1,]) %>%
          dfprint(dec) %>%
          set_colnames(c(orlab, ci_perc[1], ci_perc[2])) %>%
          set_rownames(object$coeff$`  `[-1]) %>%
          print
        cat("\n")
      } else if (object$link == "probit") {
        cat("Odds ratios are not calculated for Probit models\n\n")
      }
    }
  }

  if (!is_empty(test_var)) {
    if ("stepwise" %in% object$check) {
      cat("Model comparisons are not conducted when Stepwise has been selected.\n")
    } else {
      # sub_form <- ". ~ 1"
      sub_form <- paste(object$rvar, "~ 1")

      vars <- object$evar
      if (object$int != "" && length(vars) > 1) {
        ## updating test_var if needed
        test_var <- test_specs(test_var, object$int)
        vars <- c(vars,object$int)
      }

      not_selected <- setdiff(vars, test_var)
      if (length(not_selected) > 0) sub_form <- paste(object$rvar, "~", paste(not_selected, collapse = " + "))
      ## update with glm_sub NOT working when called from radiant - strange
      # glm_sub <- update(object$model, sub_form, data = object$model$model)
      glm_sub <- sshhr(glm(as.formula(sub_form), weights = object$wts, family = binomial(link = object$link), data = object$model$model))
      glm_sub_fit <- glance(glm_sub)
      glm_sub_test <- anova(glm_sub, object$model, test='Chi')

      # object <- list()
      # dat <- mtcars
      # dat$disp <- dat$disp > 200
      # dat$cyl365 <- dat$cyl
      # object$model <- glm(vs ~ mpg + cyl365 + disp + disp:cyl365, family = binomial(link = "logit"), data = dat)
      # glm_sub <- glm(vs ~ mpg, family = binomial(link = "logit"), data = dat)
      # glm_sub_fit <- glance(glm_sub)
      # glm_sub <- anova(glm_sub, object$model, test='Chi')
      # test_var <- "cyl:disp"
      # summary(object$model)

      # test_var <- c("cyl365","cyl:disp")
      # linearHypothesis(object$model, matchCoefs(object$model, test_var))
      # linearHypothesis(object$model, matchCf(object$model$coef, test_var))
      # cf <- object$model$coef

      matchCf <- function(clist, vlist) {

        matcher <- function(vl, cn)
        if (grepl(":", vl)) {
          strsplit(vl,":") %>%
          unlist %>%
          sapply(function(x) gsub("var",x, "((var.*:)|(:var))")) %>%
          paste0(collapse = "|") %>%
          grepl(cn) %>%
          cn[.]
        } else {
          mf <- grepl(paste0("^",vl,"$"),cn) %>% cn[.]
          if (length(mf) == 0)
            mf <- grepl(paste0("^",vl), cn) %>% cn[.]
          mf
        }

        cn <- names(clist)
        sapply(vlist, matcher, cn) %>% unname

        ## testing
        # matchCf(cl, "disp:cyl365")
        # matchCf(cl, "disp")
        # matchCf(cl, c("disp","dips:cyl365"))
      }

      if (!is_empty(object$wts, "None") && class(object$wts) != "integer") {
        ## http://stats.stackexchange.com/a/132521/61693
        # if (is.null(object$vcov))
          # object$vcov <- sandwich::vcovHC(object$model, type="HC0")
        # sub_form <- paste(test_var, "= 0")
        # glm_sub_lh <- linearHypothesis(object$model, sub_form, vcov = object$vcov)
        glm_sub_lh <- linearHypothesis(object$model,
                                       matchCf(object$model$coef, test_var),
                                       vcov = object$vcov)
        # glm_sub_lh <- linearHypothesis(object$model, sub_form, vcov = vcovHC)
        # glm_sub_lh <- linearHypothesis(object$model, glm_sub, vcov = vcovHC)
        pval <- glm_sub_lh[2,"Pr(>Chisq)"]
        df <- glm_sub_lh[2,"Df"]
        # chi2 <- qchisq(1 - pval, df)
        chi2 <- glm_sub_lh[2,"Chisq"]
      } else {
        pval <- glm_sub_test[2,"Pr(>Chi)"]
        df <- glm_sub_test[2,"Df"]
        chi2 <- glm_sub_test[2,"Deviance"]
      }

      ## pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_regression
      glm_sub_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>% round(dec)
      # glm_sub_pval <- glm_sub_test[,"Pr(>Chi)"][2] %>% { if (. < .001) "< .001" else round(., dec) }
      glm_sub_pval <- if (!is.na(pval) && pval < .001) "< .001" else round(pval, dec)
      cat(attr(glm_sub_test,"heading")[2])
      cat("\nPseudo R-squared, Model 1 vs 2:", c(glm_sub_fit$r2, glm_fit$r2))
      # cat(paste0("\nChi-statistic: ", round(glm_sub_test$Deviance[2], dec), " df(", glm_sub_test$Df[2], "), p.value ", glm_sub_pval))
      cat(paste0("\nChi-squared: ", round(chi2, dec), " df(", df, "), p.value ", glm_sub_pval))
    }
  }
}

#' Plot method for the glm_reg function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{glm_reg}}
#' @param plots Plots to produce for the specified GLM model. Use "" to avoid showing any plots (default). "hist" shows histograms of all variables in the model. "scatter" shows scatter plots (or box plots for factors) for the response variable with each explanatory variable. "dashboard" is a series of four plots used to visually evaluate model. "coef" provides a coefficient plot
#' @param conf_lev Confidence level to use for coefficient and odds confidence intervals (.95 is the default)
#' @param intercept Include the intercept in the coefficient plot (TRUE or FALSE). FALSE is the default
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' plot(result, plots = "coef")
#'
#' @seealso \code{\link{glm_reg}} to generate results
#' @seealso \code{\link{plot.glm_reg}} to plot results
#' @seealso \code{\link{predict.glm_reg}} to generate predictions
#' @seealso \code{\link{plot.glm_predict}} to plot prediction output
#'
#' @export
plot.glm_reg <- function(x,
                         plots = "",
                         conf_lev = .95,
                         intercept = FALSE,
                         shiny = FALSE,
                         ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)

  if (class(object$model)[1] != 'glm') return(object)

  if (plots[1] == "")
    return(cat("Please select a glm regression plot from the drop-down menu"))

  model <- ggplot2::fortify(object$model)
  model$.fitted <- predict(object$model, type = 'response')
  model$.actual <- as.numeric(object$rv)
  model$.actual <- model$.actual - max(model$.actual) + 1   # adjustment in case max > 1

  rvar <- object$rvar
  evar <- object$evar
  vars <- c(object$rvar, object$evar)
  nrCol <- 2
  plot_list <- list()

  ## use orginal data rather than the logical used for estimation
  model[[rvar]] <- object$rv

  if ("hist" %in% plots) {
    for (i in vars)
      plot_list[[i]] <- visualize(select_(model, .dots = i), xvar = i, bins = 10, custom = TRUE)
  }

  if ("coef" %in% plots) {

    ## no plots if aliased coefficients present
    # if (anyNA(object$model$coeff))
      # return("Model has missing coefficient(s) because the set of explanatory variables\nexhibit perfect multicollinearity. No plot shown.")

    nrCol <- 1
    if (!is_empty(object$wts, "None") && class(object$wts) != "integer")
      cnfint <- radiant::confint_robust
    else
      cnfint <- confint.default

    plot_list[["coef"]] <- cnfint(object$model, level = conf_lev, vcov = object$vcov) %>%
          exp %>%
          data.frame %>%
          na.omit %>%
          set_colnames(c("Low","High")) %>%
          cbind(select(object$coeff,2),.) %>%
          set_rownames(object$coeff$`  `) %>%
          { if (!intercept) .[-1,] else . } %>%
          mutate(variable = rownames(.)) %>%
          ggplot() +
            geom_pointrange(aes_string(x = "variable", y = "OR", ymin = "Low", ymax = "High")) +
            geom_hline(yintercept = 1, linetype = 'dotdash', color = "blue") +
            ylab("Odds-ratio") +
            ## can't use coord_trans together with coord_flip
            # http://stackoverflow.com/a/26185278/1974918
            scale_y_log10(breaks = c(0,0.5,1:10)) +
            coord_flip()
  }

  if (plots == "scatter") {
    for (i in evar) {
      if ("factor" %in% class(model[,i])) {
        plot_list[[i]] <- ggplot(model, aes_string(x=i, fill=rvar)) +
                        geom_bar(position = "fill", alpha=.5) +
                        labs(list(y = ""))
      } else {
        plot_list[[i]] <-
          visualize(select_(model, .dots = c(i,rvar)), xvar = rvar, yvar = i, check = "jitter", type = "scatter", custom = TRUE)
      }
    }
    nrCol <- 1
  }

  if (plots == "dashboard") {

    plot_list[[1]] <-
      visualize(model, xvar = ".fitted", yvar = ".actual", type = "scatter", check = "jitter", custom = TRUE) +
      stat_smooth(method="glm", method.args = list(family = "binomial"), se=TRUE) +
      labs(list(title = "Actual vs Fitted values", x = "Fitted", y = "Actual"))

    # plot_list[[1]] <- ggplot(model, aes_string(x=".fitted", y=".actual")) + geom_point(alpha = .25) +
    #        stat_smooth(method="glm", method.args = list(family = "binomial"), se=TRUE) +
    #        geom_jitter(position = position_jitter(height = .05)) +
    #        labs(list(title = "Actual vs Fitted values", x = "Fitted values", y = "Actual"))

    # plot_list[[2]] <- ggplot(model, aes_string(x=".fitted", y=".resid")) + geom_point(alpha = .25) +
    #        geom_hline(yintercept = 0) + geom_smooth(size = .75, linetype = "dotdash", se = TRUE) +
    #        labs(list(title = "Residuals vs Fitted values", x = "Fitted", y = "Residuals"))

    plot_list[[2]] <-
      visualize(model, xvar = ".fitted", yvar = ".resid", type = "scatter", custom = TRUE) +
      labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")) +
      geom_hline(yintercept = 0)
      # + geom_smooth(size = .75, linetype = "dotdash", se = TRUE)

    plot_list[[3]] <-
      visualize(model, xvar = ".resid", custom = TRUE) +
      labs(list(title = "Histogram of residuals", x = "Residuals"))

    plot_list[[4]] <- ggplot(model, aes_string(x=".resid")) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(model[,".resid"]), sd = sd(model[,".resid"])), color = "blue") +
      labs(list(title = "Residual vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())
  }

  if (length(plot_list) > 0) {
    sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = nrCol))) ) %>%
      { if (shiny) . else print(.) }
  }
}

#' Predict method for the glm_reg function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{glm_reg}}
#' @param pred_vars Variables selected to generate predictions
#' @param pred_data Provide the name of a dataframe to generate predictions (e.g., "titanic"). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable use a `,` (e.g., `pclass = levels(pclass), age = seq(0,100,20)`)
#' @param prn Number of lines of prediction results to print. Nothing is printed if prn is 0. Use -1 to print all lines (default).
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), lev = "Yes")
#'  predict(result, pred_cmd = "pclass = levels(pclass)")
#' glm_reg("titanic", "survived", c("pclass","sex"), lev = "Yes") %>%
#'   predict(pred_cmd = "sex = c('male','female')")
#' glm_reg("titanic", "survived", c("pclass","sex"), lev = "Yes") %>%
#'  predict(pred_data = "titanic")
#'
#' @seealso \code{\link{glm_reg}} to generate the result
#' @seealso \code{\link{summary.glm_reg}} to summarize results
#' @seealso \code{\link{plot.glm_reg}} to plot results
#' @seealso \code{\link{plot.glm_predict}} to plot prediction output
#'
#' @export
predict.glm_reg <- function(object,
                            pred_vars = "",
                            pred_data = "",
                            pred_cmd = "",
                            prn = 100,
                            se = FALSE,
                            ...) {

  if (is.character(object)) return(object)
  pred_count <- sum(c(pred_vars == "", pred_cmd == "", pred_data == ""))
  ## used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression as starting point
  if ("standardize" %in% object$check) {
    return(cat("Standardized coefficients cannot be used for prediction.\nPlease uncheck the standardized coefficients box and try again"))
  } else if (pred_count == 3) {
    return("Please specify a command to generate predictions. For example,\n pclass = levels(pclass) would produce predictions for the different\n levels of factor pclass. To add another variable use a ,\n(e.g., pclass = levels(pclass), age = seq(0,100,20))\n\nMake sure to press return after you finish entering the command. If no\nresults are shown the command was invalid. Alternatively specify a dataset\nto generate predictions. You could create this in a spreadsheet and use the\nclipboard feature in Data > Manage to bring it into Radiant" %>% set_class(c("glm_predict",class(.))))
  }

  dec <- object$dec

  # if (pred_count < 2) {
  #   if (pred_cmd != "")
  #     cat("Multiple inputs where specified for prediciton. The command will be used.\nTo use variables or a dataset remove the command.")
  #   if (pred_vars != "")
  #     cat("Multiple inputs where specified for prediciton. The variables selected will be used.\nTo use a command or dataset unselect variables.")
  # }

  pred_type <- "cmd"
  vars <- object$evar
  if (pred_data == "" && pred_cmd != "") {
    pred_cmd %<>% gsub("\"","\'",.) %>% gsub(";\\s*$","",.) %>% gsub(";",",",.)
    pred <- try(eval(parse(text = paste0("with(object$model$model, expand.grid(", pred_cmd ,"))"))), silent = TRUE)
    if (is(pred, 'try-error')) {
      return(paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred,"condition")$message, "\n\nPlease try again. Examples are shown in the help file.") %>% set_class(c("glm_predict",class(.))))
    }

    ## adding information to the prediction data.frame
    dat_classes <- attr(object$model$term, "dataClasses")[-1]

    ## weights mess-up data manipulation below so remove from
    wid <- which(names(dat_classes) %in% "(weights)")
    if (length(wid) > 0) dat_classes <- dat_classes[-wid]

    # isFct <- dat_classes == "factor" | dat_classes == "logical"
    isFct <- dat_classes == "factor"
    isLog <- dat_classes == "logical"
    isNum <- dat_classes == "numeric"
    dat <- select_(as.data.frame(object$model$model), .dots = vars)

    # print(dat_classes)
    # print(vars)

    # print(sum(isNum) + sum(isFct) + sum(isLog))
    # print(sum(isNum) + sum(isFct))
    # print(length(vars))

    ## based on http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
    max_freq <- function(x) names(which.max(table(x)))
    max_lfreq <- function(x) ifelse(mean(x) > .5, TRUE, FALSE)

    plug_data <- data.frame(init___ = 1)
    if (sum(isNum) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(mean), vars[isNum]))
    if (sum(isFct) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(max_freq), vars[isFct]))
    if (sum(isLog) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(max_lfreq), vars[isLog]))

    rm(dat)

    # if ((sum(isNum) + sum(isFct) + sum(isLog)) < length(vars)) {
    if ((sum(isNum) + sum(isFct) + sum(isLog)) < length(vars)) {
      return("The model includes data-types that cannot be used for\nprediction at this point\n" %>%
        set_class(c("glm_predict",class(.))))

    } else {
      if (sum(names(pred) %in% names(plug_data)) < length(names(pred))) {
        return("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n" %>% set_class(c("glm_predict",class(.))))
      } else {
        plug_data[names(pred)] <- list(NULL)
        pred <- data.frame(plug_data[-1],pred)
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

    if (pred_cmd != "") {
      pred_cmd <- pred_cmd %>% gsub("\"","\'",.) %>% gsub(" ","",.)
      vars <-
        strsplit(pred_cmd, ";")[[1]] %>% strsplit(., "=") %>%
        sapply("[", 1) %>% gsub(" ","",.)
      dots <- strsplit(pred_cmd, ";")[[1]] %>% gsub(" ","",.)
      for (i in seq_along(dots))
        dots[[i]] <- sub(paste0(vars[[i]],"="),"",dots[[i]])

      pred <- try(mutate_(pred, .dots = setNames(dots, vars)), silent = TRUE)
      if (is(pred, 'try-error')) {
        return(paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred,"condition")$message, "\n\nPlease try again. Examples are shown in the help file.") %>% set_class(c("glm_predict",class(.))))
        return()
      }
      pred_type <- "datacmd"
    } else {
      pred_type <- "data"
    }

    pred %<>% na.omit()
  }

  pred_val <- try(sshhr(predict(object$model, pred, type = 'response', se.fit = se)), silent = TRUE)
  if (!is(pred_val, 'try-error')) {
    if (se) {
      pred_val %<>% data.frame %>% select(1:2)
      colnames(pred_val) <- c("Prediction","std.error")
    } else {
      pred_val %<>% data.frame %>% select(1)
      colnames(pred_val) <- "Prediction"
    }
    pred <- data.frame(pred, pred_val, check.names = FALSE)

    if (prn == TRUE || prn != 0) {
      cat("Generalized linear model (GLM)")
      cat("\nLink function:", object$link)
      cat("\nData         :", object$dataset)
      if (object$data_filter %>% gsub("\\s","",.) != "")
        cat("\nFilter       :", gsub("\\n","", object$data_filter))
      cat("\nResponse variable    :", object$rvar)
      cat("\nLevel                :", object$lev, "in", object$rvar)
      cat("\nExplanatory variables:", paste0(object$evar, collapse=", "),"\n\n")

      if (pred_type == "cmd") {
        cat("Predicted values for:\n")
      } else if (pred_type == "datacmd") {
        cat(paste0("Predicted values for profiles from dataset: ", pred_data,"\n"))
        cat(paste0("Customized using command: ", pred_cmd, "\n"))
      } else {
        cat(paste0("Predicted values for profiles from dataset: ", pred_data,"\n"))
      }

      if (is.logical(prn) || prn == -1) {
        cat("\n")
        dfprint(pred, dec) %>% print(row.names = FALSE)
      } else {
        if (nrow(pred) > prn)
          cat(paste0("Number of rows shown: ", prn, "\n\n"))
        head(pred, prn) %>% dfprint(dec) %>% print(row.names = FALSE)
      }
    }

    return(pred %>% set_class(c("glm_predict",class(.))))
  } else {
    return(paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred_val,"condition")$message, "\n\nPlease try again. Examples are shown in the help file.") %>% set_class(c("glm_predict",class(.))))
  }

  return(invisible())
}

#' Plot method for the predict.glm_reg function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{predict.glm_reg}}.
#' @param xvar Variable to display along the X-axis of the plot
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different colour
#' @param conf_lev Confidence level to use for prediction intervals (.95 is the default). Note that the error bars for predicitions are approximations at this point.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex","age"), lev = "Yes")
#' pred <- predict(result, pred_cmd = "pclass = levels(pclass)")
#' plot(pred, xvar = "pclass")
#' pred <- predict(result, pred_cmd = "age = 0:100")
#' plot(pred, xvar = "age")
#' pred <- predict(result, pred_cmd = "pclass = levels(pclass), sex = levels(sex)")
#' plot(pred, xvar = "pclass", color = "sex")
#' pred <- predict(result, pred_cmd = "pclass = levels(pclass), age = seq(0,100,20)")
#' plot(pred, xvar = "pclass", color = "age")
#' plot(pred, xvar = "age", color = "pclass")
#' pred <- predict(result, pred_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,20)")
#' plot(pred, xvar = "age", color = "sex", facet_col = "pclass")
#' plot(pred, xvar = "age", color = "pclass", facet_col = "sex")
#' pred <- predict(result, pred_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,5)")
#' plot(pred, xvar = "age", color = "sex", facet_col = "pclass")
#' plot(pred, xvar = "age", color = "pclass", facet_col = "sex")
#'
#' @seealso \code{\link{glm_reg}} to generate the result
#' @seealso \code{\link{summary.glm_reg}} to summarize results
#' @seealso \code{\link{plot.glm_reg}} to plot results
#' @seealso \code{\link{predict.glm_reg}} to generate predictions
#'
#' @export
plot.glm_predict <- function(x,
                             xvar = "",
                             facet_row = ".",
                             facet_col = ".",
                             color = "none",
                             conf_lev = .95,
                             ...) {

  if (is.null(xvar) || xvar == "") return(invisible())

  object <- x; rm(x)
  if (is.character(object)) return(invisible())

  # object$ymin <- object$Prediction - qnorm(.5 + conf_lev/2)*object$std.error
  # object$ymax <- object$Prediction + qnorm(.5 + conf_lev/2)*object$std.error

  byvar <- NULL
  if (color != "none") byvar <- color
  if (facet_row != ".")
    byvar <- if (is.null(byvar)) facet_row else unique(c(byvar, facet_row))

  if (facet_col != ".")
    byvar <- if (is.null(byvar)) facet_col else unique(c(byvar, facet_col))

  tbv <- if (is.null(byvar)) xvar else c(xvar, byvar)
  # tmp <- object %>% group_by_(.dots = tbv) %>% select_(.dots = c("Prediction","ymin","ymax")) %>% summarise_each(funs(mean))
  tmp <- object %>% group_by_(.dots = tbv) %>% select_(.dots = "Prediction") %>% summarise_each(funs(mean))
  if (color == 'none') {
    p <- ggplot(tmp, aes_string(x=xvar, y="Prediction")) + geom_line(aes(group = 1))
  } else {
    p <- ggplot(tmp, aes_string(x=xvar, y="Prediction", color = color, group = color)) + geom_line()
  }

  if (facet_row != "." || facet_col != ".") {
    facets <- if (facet_row == ".")  paste("~", facet_col)
              else paste(facet_row, '~', facet_col)
    facet_fun <- if (facet_row == ".") facet_wrap else facet_grid
    p <- p + facet_fun(as.formula(facets))
  }

  # if (is.factor(tmp[[xvar]]) || length(unique(tmp[[xvar]])) < 10)
  #   p <- p + geom_pointrange(aes_string(ymin = "ymin", ymax = "ymax"), size=.3)
  # else
  #   p <- p + geom_smooth(aes_string(ymin = "ymin", ymax = "ymax"), stat="identity")

  sshhr( p )
}

#' Store residuals or predicted values generated in the glm_reg function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{glm_reg}} or \code{\link{predict.glm_reg}}
#' @param data Dataset name
#' @param type Residuals ("residuals") or predictions ("predictions"). For predictions the dataset name must be provided
#' @param name Variable name assigned to the residuals or predicted values
#'
#' @examples
#' \dontrun{
#' result <- glm_reg("titanic", "survived", "pclass", lev = "Yes")
#' store_glm(result)
#' }
#' @export
store_glm <- function(object,
                      data = object$dataset,
                      type = "residuals",
                      name = paste0(type, "_glm")) {

  # if (!is.null(object$data_filter) && object$data_filter != "")
  # if (!is_empty(object$data_filter))
  #   return(message("Please deactivate data filters before trying to store predictions or residuals"))

  ## fix empty name input
  if (gsub("\\s","",name) == "") name <- paste0(type, "_glm")

  if (type == "residuals") {
    store <- object$model$residuals
  } else {
    ## gsub needed because trailing/leading spaces may be added to the variable name
    name <- unlist(strsplit(name, ",")) %>% gsub("\\s","",.)
    if (length(name) > 1) {
      name <- name[1:min(2, length(name))]
      ind <- which(colnames(object) == "Prediction") %>% {.:(. + length(name[-1]))}
      store <- object[,ind]
    } else {
      store <- object$Prediction
    }
  }

  changedata(data, vars = store, var_names = name)
}

#' Confidence interval for robust estimators
#'
#' @details Wrapper for confint.default with robust standard errors. See \url{http://stackoverflow.com/a/3820125/1974918}
#'
#' @param object A fitted model object
#' @param parm A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered
#' @param level The confidence level required
#' @param vcov Covariance matrix generated by, e.g., sandwich::vcovHC
#' @param ... Additional argument(s) for methods
#'
#' @importFrom sandwich vcovHC
#'
#' @export
confint_robust <- function (object, parm, level = 0.95, vcov = NULL, ...) {
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    # pct <- format.perc(a, 3)
    fac <- qnorm(a)
    # ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, c("Low","High")))

    if (is.null(vcov))
      vcov <- sandwich::vcovHC(object, type="HC0")
    ses <- sqrt(diag(vcov))[parm]
    # coeff$std.error <- sqrt(diag(sandwich::vcovHC(model, type="HC0")))
    ci[] <- cf[parm] + ses %o% fac
    ci
}
