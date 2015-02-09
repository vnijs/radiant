###############################
# Add coeficient plots
###############################
#
# customer.stats <- data.frame(b = lm1.c$coefficient[weekday.par.names],
#                              ci.lower=confint(lm1.c,weekday.par.names)[,1],
#                              ci.upper=confint(lm1.c,weekday.par.names)[,2],
#                              weekday=factor(weekday.names,levels=weekday.names),
#                              usertype=rep('Customer',length(weekday.names)))
#
# sub.stats <- data.frame(b = lm1.s$coefficient[weekday.par.names],
#                         ci.lower=confint(lm1.s,weekday.par.names)[,1],
#                         ci.upper=confint(lm1.s,weekday.par.names)[,2],
#                         weekday=factor(weekday.names,levels=weekday.names),
#                         usertype=rep('Subscriber',length(weekday.names)))
#
# reg.stats <- rbind(customer.stats,sub.stats)
#
# weekday.effects.plot <- ggplot(data=reg.stats) +
#   geom_pointrange(aes(x=weekday,y=b,ymin=ci.lower,ymax=ci.upper,color=usertype)) +
#   geom_line(aes(x=weekday,y=b,color=usertype,group=usertype),linetype='dotted') +
#   geom_hline(aes(yintercept=0))+
#   facet_wrap(~usertype,scales='free_y')

################################################################
# Regression
################################################################
output$uiReg_var1 <- renderUI({
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  selectInput(inputId = "reg_var1", label = "Dependent variable:", choices = vars,
  	selected = state_singlevar("reg_var1",vars), multiple = FALSE)
})

output$uiReg_var2 <- renderUI({
	notChar <- "character" != getdata_class()
  vars <- varnames()[notChar]
  if(length(vars) > 0 ) vars <- vars[-which(vars == input$reg_var1)]
  selectInput(inputId = "reg_var2", label = "Independent variables:", choices = vars,
    # the reference to reg_var2 below should help ensure that variables
    # remain selected even if the dv changes
  	selected = state_init_multvar("reg_var2", isolate(input$reg_var2),vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$uiReg_var3 <- renderUI({
	if(input$reg_var2 %>% not_available) return()
  vars <- varnames()
 	vars <- vars[which(vars %in% input$reg_var2)]

  # adding interaction terms as needed
	if(!is.null(input$reg_intsel) && input$reg_interactions != 'none')
		vars <- c(vars,input$reg_intsel)

	# use selectize for reg_var3 or not?
	# selectInput(inputId = "reg_var3", label = "Variables to test:", choices = vars,
  selectizeInput(inputId = "reg_var3", label = "Variables to test:", choices = vars,
  	selected = state_multvar("reg_var3", vars), multiple = TRUE,
    options = list(placeholder = 'None', plugins = list('remove_button'))
  )
})

output$uiReg_intsel <- renderUI({
  vars <- input$reg_var2
	if(vars %>% not_available || length(vars) < 2) return()
 	choices <- reg_int_vec(vars, input$reg_interactions)
	selectInput("reg_intsel", label = "", choices = choices,
  	selected = state_multvar("reg_intsel", vars), multiple = TRUE, selectize = FALSE)
})

reg_interactions <- c("None" = "none", "2-way" = "2way", "3-way" = "3way")
# reg_predict_cmd <- c("Data" = "dataframe","Command" = "cmd")
reg_predict <- c("None" = "none", "Data" = "data","Command" = "cmd")
reg_check <- c("RMSE" = "rmse", "Sum of squares" = "sumsquares",
               "VIF" = "vif", "Confidence intervals" = "confint",
               "Standardized coefficients" = "standardize",
               "Stepwise selection" = "stepwise")

output$ui_regression <- renderUI({
  list(
  	wellPanel(
		  conditionalPanel(condition = "input.tabs_regression == 'Plots'",
		    selectInput("reg_plots", "Regression plots:", choices = r_plots,
			  	selected = state_init_list("reg_plots","", r_plots)),
		    checkboxInput('reg_line', 'Line', value = state_init("reg_line", FALSE)),
		    checkboxInput('reg_loess', 'Loess', value = state_init("reg_loess", FALSE))
		  ),
	    uiOutput("uiReg_var1"),
	    uiOutput("uiReg_var2"),
		  radioButtons(inputId = "reg_interactions", label = "Interactions:", reg_interactions,
	    	selected = state_init_list("reg_interactions","none", reg_interactions),
	    	inline = TRUE),
		  conditionalPanel(condition = "input.reg_interactions != 'none'",
				uiOutput("uiReg_intsel")
			),
		  conditionalPanel(condition = "input.tabs_regression == 'Summary'",
   	    radioButtons(inputId = "reg_predict", label = "Prediction:", reg_predict,
	      	selected = state_init_list("reg_predict","dataframe",reg_predict),
	      	inline = TRUE),

        conditionalPanel(condition = "input.reg_predict == 'cmd'",
          returnTextInput("reg_predict_cmd", "Predict (e.g., carat = seq(.5,1,.05))",
	    		  value = state_init('reg_predict_cmd',''))
        ),
        conditionalPanel(condition = "input.reg_predict == 'data'",
          selectizeInput(inputId = "reg_predict_data", label = "Predict for profiles:",
                      choices = c("None" = "none",r_data$datasetlist),
                      selected = state_init("reg_predict_data"), multiple = FALSE)
        ),
		    uiOutput("uiReg_var3"),

        checkboxGroupInput("reg_check", NULL, reg_check,
          selected = state_init_list("reg_check","", reg_check), inline = TRUE),
        br(),
        checkboxInput(inputId = "reg_rmse", label = "RMSE",
	    		value = state_init('reg_rmse',FALSE)),
        checkboxInput(inputId = "reg_sumsquares", label = "Sum of squares",
	    		value = state_init('reg_sumsquares',FALSE)), br(),
  	    checkboxInput(inputId = "reg_vif", label = "VIF",
	    		value = state_init('reg_vif',FALSE)),
        checkboxInput(inputId = "reg_confint", label = "Confidence intervals",
      		value = state_init('reg_confint',FALSE)),
        checkboxInput(inputId = "reg_standardize", label = "Standardized coefficients",
     		  value = state_init('reg_standardize',FALSE)),
        checkboxInput(inputId = "reg_stepwise", label = "Select variables step-wise",
      		value = state_init('reg_stepwise',FALSE)),

		    conditionalPanel(condition = "input.reg_confint == true |
		                     input.reg_predict_data != 'none' |
		                     input.reg_predict != ''",
           sliderInput('reg_conf_level',"Adjust confidence level:", min = 0.70, max = 0.99,
                       value = state_init('reg_conf_level',.95), step = 0.01)
		    )
			),
		  actionButton("saveres", "Save residuals")
	  ),
		helpAndReport('Regression','regression', inclRmd("../quant/tools/help/regression.Rmd"))
		# helpAndReport('Regression','regression', paste(readLines("../quant/tools/help/regression.html", warn = FALSE), collapse = '\n'))
	)
})

reg_plotWidth <- function() {
	result <- .regression()
	ifelse(class(result) == 'lm', return(result$plotWidth), return(650))
}

reg_plotHeight <- function() {
	result <- .regression()
	ifelse(class(result) == 'lm', return(result$plotHeight), return(650))
}

output$regression <- renderUI({
	# create inputs and outputs - function in radiant.R
  statTabPanel("Regression","Linear (OLS)",".regression","regression", "reg_plotWidth", "reg_plotHeight")
})

# regression <- function(dataset, reg_var1, reg_var2, reg_var3, reg_intsel, reg_interactions,
#                        reg_predict, reg_predict_cmd, reg_predict_data, reg_standardize, reg_sumsquares,
#                        reg_confint, reg_conf_level, reg_rmse, reg_vif, reg_stepwise, reg_plots, reg_line,
#                        reg_loess) {

# reg_var1 = reg_dep_var
# reg_var2 = reg_indep_var
# reg_var3 = reg_test_var
# reg_interactions = reg_intsel
# reg_int_var = reg_interactions

regression <- function(dataset, reg_var1, reg_var2,
                       data_filter = "",
                       show_filter = FALSE, 		# remove when recode is complete
                       reg_var3 = "",
                       reg_intsel = "",
                       reg_interactions = "none",
                       reg_predict = "none",
                       reg_predict_cmd = "",
                       reg_predict_data = "",
                       reg_standardize = FALSE,
                       reg_sumsquares = FALSE,
                       reg_confint = FALSE,
                       reg_conf_level = .95,
                       reg_rmse = FALSE,
                       reg_vif = FALSE,
                       reg_stepwise = FALSE,
                       reg_plots = "",
                       reg_line = FALSE,
                       reg_loess = FALSE) {

	vars <- reg_var2

	# adding interaction terms as needed
	if(reg_interactions != 'none' && !is.null(reg_intsel) && length(vars) > 1) {
		vars <- c(vars,reg_intsel)
	}

	# dat <- r_data[[dataset]]
  # dat <- select_(values[["mtcars"]], .dots = c(reg_dep_var, reg_indep_var))
	dat <- getdata_exp(dataset, c(reg_var1, reg_var2), filt = data_filter)

	if(reg_standardize) dat <- mutate_each(dat,funs(reg_standardize_fun))

	formula <- paste(reg_var1, "~", paste(vars, collapse = " + "))

	if(reg_stepwise) {
    # use k = 2 for AIC, use k = log(nrow(dat)) for BIC
		mod <- lm(paste(reg_dep_var, "~ 1") %>% as.formula, data = dat) %>%
      step(., k = 2, scope = list(upper = formula), direction = 'both')
	} else {
		mod <- lm(formula, data = dat)
	}

	### Check if variables were dropped?
  # if (nzchar(mess <- naprint(x$na.action)))
  #   cat("  (", mess, ")\n", sep = "")



	# specifying plot heights
	nrVars <- length(as.character(attr(mod$terms,'variables'))[-1])

	plotHeight <- 650
	plotWidth <- 650

	if(reg_plots == 'histlist') plotHeight <- 325 * ceiling(nrVars / 2)

	if(reg_plots == 'dashboard') plotHeight <- 630 + 375

	if(reg_plots == 'correlations') {
		plotHeight <- 150 * nrVars
		plotWidth <- 150 * nrVars
	}

	if(reg_plots %in% c('scatterlist','leverage_plots','resid_vs_predictorlist'))
		plotHeight <- 325 * ceiling((nrVars-1) / 2)

  environment() %>% as.list %>% set_class(c("regression",class(.)))
}

# summary_regression <- reactive({ .summary_regression() })
summary_regression <- function(result = .regression()) {

	if(class(result$mod) != 'lm') return(result)

	# sig_stars <- function(pval) {
	#   sapply(pval, function(x) x < c(.001,.01, .05, .1)) %>%
	#     colSums %>% add(1) %>%
	#     c("",".","*", "**", "***")[.]
	# }

	reg_coeff <- tidy(result$mod)
	reg_coeff$` ` <- sig_stars(reg_coeff$p.value)
	reg_coeff[,c(2:5)] %<>% round(3)
	reg_coeff$p.value[reg_coeff$p.value < .001] <- "< .001"
	colnames(reg_coeff) <- c("  ","coefficient","std.error","t.value","p.value"," ")
	print(reg_coeff, row.names=FALSE)

	reg_fit <- glance(result$mod) %>% round(3)
	if(reg_fit['p.value'] < .001) reg_fit['p.value'] <- "< .001"
	# reg_fit

	reg_aug <- augment(result$mod)
	# reg_aug[1:10,]

  # cat(paste("Data:",dataset,"\n"))
  # cat(paste("Dependent variable:",dv,"\n"))

  # if(std_c == TRUE) {
  #   cat("Standardized coefficients:\n")
  # } else {
  #   cat("Coefficients:\n")
  # }

  # if (!is.null(x$fstatistic)) {
  #   cat("R-squared: ", formatC(x$r.squared, digits = digits))
  #   cat(", Adjusted R-squared: ", formatC(x$adj.r.squared, digits = digits),
  #       "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits), "on", x$fstatistic[2L],
  #       "and", x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),  digits = digits)
  #   )
  #   cat(paste0("\nNr obs: ",length(x$residuals)))
  #   cat("\n\n")
  # }



	# if(reg_outlier) print(outlierTest(result), digits = 3)

  if(result$reg_predict_cmd != '' || result$reg_predict_data != "none") {

    # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
    # as starting point
    if(result$reg_standardize) {
      cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the
          standardized coefficients box and try again.")
    } else {

      if(result$reg_predict == "cmd") {
     		reg_predict_cmd <- gsub("\"","\'", result$reg_predict_cmd)
        nval <- try(eval(parse(text = paste0("data.frame(",reg_predict_cmd,")"))), silent = TRUE)
      } else {
        nval <- r_data[[result$reg_predict_data]]
        vars <- as.character(attr(result$terms,'variables'))[-1]
        nval <- try(select_(nval, .dots = vars[-1]), silent = TRUE)
      }

      if(is(nval, 'try-error')) {
        if(result$reg_predict == "cmd") {
          cat("The expression entered does not seem to be correct. Please try again.\n")
          cat("Examples are shown in the helpfile.\n")
        } else {
          cat("The profiles to predict do not contain all variables that are in the model.\n")
          cat("Add variables to the profiles data as needed.\n\n")
          ivars <- vars[-1]
          cat("Model variables: ")
          cat(ivars,"\n")
          cat("Profile variables to be added: ")
          nval_names <- names(r_data[[result$reg_predict_data]])
          cat(ivars[!ivars %in% nval_names])
        }
      } else {

        dat <- ggplot2::fortify(result)
        vars <- as.character(attr(result$terms,'variables'))[-1]
        reg_var1 <- vars[1]
        reg_var2 <- vars[-1]
        dat <- dat[,reg_var2, drop = FALSE]

        isFct <- sapply(dat, is.factor)
        isNum <- sapply(dat, is.numeric)

        if(sum(isNum) + sum(isFct) < dim(dat)[2]) {
          cat("The model includes data-types that cannot be used for\nprediction at this point\n")
        } else {

          newdat <- ""
          if(sum(isNum) > 0)  newdat <- data.frame(newdat,t(colMeans(dat[,isNum, drop = FALSE])))
          # from http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
          if(sum(isFct) > 0)  newdat <- data.frame(newdat,t(apply(dat[,isFct, drop = FALSE],2,function(x) names(which.max(table(x))))))

          # if(sum(names(nval) %in% names(newdat)) < length(nval)) {
          if(sum(names(nval) %in% names(newdat)) < length(names(nval))) {
            cat("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n")
          } else {
            if(result$reg_predict == "cmd" & result$reg_predict_cmd == "") {
              pred <- try(log("a"), silent=TRUE)
            } else {
              newdat[names(nval)] <- list(NULL)
              nnd <- data.frame(newdat[-1],nval)
              pred <- try(predict(result, nnd,interval = 'prediction', level = result$reg_conf_level), silent = TRUE)
              # pred <- try(predict(result, nnd,interval = 'prediction', level = as.numeric(reg_conf_level)), silent = TRUE)
            }

            if(!is(pred, 'try-error')) {
            	if(result$reg_predict == "dataframe") {
              	cat(paste0("Predicted values for profiles from dataset: ",result$reg_predict_data,"\n"))
              } else {
              	cat("Predicted values for:\n")
              }

            	pred <- data.frame(pred,pred[,3]-pred[,1])
              cl_split <- function(x) 100*(1-x)/2
            	cl_split(result$reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
            	(100 - cl_split(result$reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high
            	colnames(pred) <- c("Prediction",cl_low,cl_high,"+/-")

            	nnd <- data.frame(nnd, pred, check.names = FALSE)

            	# putting the predictions into the clipboard
            	os_type <- .Platform$OS.type
            	if (os_type == 'windows') {
            	  write.table(nnd, "clipboard", sep="\t", row.names=FALSE)
            	} else {
            	  write.table(nnd, file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
            	}

            	nnd %>% print(., row.names = FALSE)
              cat("\n")
            } else {
              cat("The expression entered does not seem to be correct. Please try again.\nExamples are shown in the helpfile.\n")
            }
          }
        }
      }
    }
  }

  if(result$reg_rmse) {
    rmse <- sqrt(mean(res$residual^2,na.rm=TRUE))
    # rmse_df <- data.frame("RMSE" = rmse, "RMSE (95%)" = rmse *2, check.names = FALSE)
    cat("Prediction error (RMSE): ", rmse, "\n\n")
	}

  if(result$reg_sumsquares) {
	  atab <- anova(result)
	  nr_rows <- dim(atab)[1]
	  df_reg <- sum(atab$Df[-nr_rows])
	  df_err <- sum(atab$Df[nr_rows])
	  df_tot <- df_reg + df_err

	  ss_reg <- sum(atab$`Sum Sq`[-nr_rows])
	  ss_err <- sum(atab$`Sum Sq`[nr_rows])
	  ss_tot <- ss_reg + ss_err
	  ssTable <- data.frame(matrix(nrow = 3, ncol = 2))
	  rownames(ssTable) <- c("Regression","Error","Total")
	  colnames(ssTable) <- c("df","SS")
	  ssTable$df <- c(df_reg,df_err,df_tot)
	  ssTable$SS <- c(ss_reg,ss_err,ss_tot)
    cat("Sum of squares:\n")
    print(format(ssTable, scientific = FALSE))
    cat("\n")
  }

  if(result$reg_vif) {
		print(vif_regression(result), digits = 3)
    cat("\n")
	}

	if(result$reg_confint) {

    cl_split <- function(x) 100*(1-x)/2
    cl_split(result$reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
    (100 - cl_split(result$reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

    cat("Coefficient confidence intervals:\n")
    confint(result, level = result$reg_conf_level) %>%
      data.frame %>%
      magrittr::set_colnames(c("Low","High")) %>%
      cbind(Estimate = result$coefficients,.) -> dat

    dat$`+/-` <- dat$High - dat$Estimate
    dat %>%
      magrittr::set_colnames(c("Estimate",cl_low,cl_high, "+/-")) %>%
      print
    cat("\n")
	}

	if(!is.null(result$reg_var3)) {
		if(!result$reg_stepwise) {
      test_regression(result)
      # NA's shown
      # print(format(test_regression(result), scientific = FALSE))
		} else {
	  	cat("Model comparisons not conducted when Stepwise has been selected.\n")
	  }
	}
}

# main functions called from radiant.R
r_plots <- list("None" = "", "Histograms" = "histlist", "Correlations" = "correlations", "Scatter" = "scatterlist", "Dashboard" = "dashboard",
		# "Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots", "Coefficient plot" = "coef")
		"Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots")

plots_regression <- function(result = .regression()) {

	if(class(result$mod) != 'lm') return(result)
	mod <- ggplot2::fortify(result$mod)

	vars <- as.character(attr(result$mod$terms,'variables'))[-1]
	reg_var1 <- vars[1]
	reg_var2 <- vars[-1]

	dat <- mod[,vars, drop = FALSE]

	if(result$reg_plots == "")
		return(plot(x = 1, type = 'n', main="Please select a plot from the Regression plots dropdown menu.", axes = FALSE, xlab = "", ylab = ""))

	if(result$reg_plots == "histlist") {
		plots <- list()
		for(i in vars) plots[[i]] <- ggplot(dat, aes_string(x = i)) + geom_histogram()
	}

	if(result$reg_plots == "correlations") {
		datlist <- list()
		datlist$dat <- dat
		return(plots_correlation(datlist))
	}

	if(result$reg_plots == "dashboard") {

		plots <- list()
		df <- data.frame(cbind(mod$.fitted,mod[1]))
		colnames(df) <- c("x","y")
		# plots[[1]] <- ggplot(df, aes(x=x, y=y)) + geom_point() + labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual values"))
		p <- ggplot(df, aes(x=x, y=y)) + geom_point() + labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual values"))
    if(result$reg_line) p <- p + geom_abline(linetype = 'dotdash')
    if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
    plots[[1]] <- p

		p <- qplot(.fitted, .resid, data = mod) + labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))
    if(result$reg_line) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75, linetype = "dashed", colour = 'black')
    if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
    plots[[2]] <- p

		#	p <- qplot(y=.resid, x=seq_along(.resid), data = mod) + geom_point() +
		p <- qplot(y=.resid, x=seq_along(.resid), data = mod, geom="line") +
			labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))
    if(result$reg_line) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75, linetype = "dashed", colour = 'black')
    if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
    plots[[3]] <- p

		p <- qplot(sample =.stdresid, data = mod, stat = "qq") +
			labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))
    if(result$reg_line) p <- p + geom_abline(linetype = 'dotdash')
		# if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
    plots[[4]] <- p

	plots[[5]] <- ggplot(mod, aes(x = .resid)) + geom_histogram() +
			labs(list(title = "Histogram of residuals", x = "Residuals"))

  plots[[6]] <- ggplot(mod, aes(x=.resid)) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(mod[,'.resid']), sd = sd(mod[,'.resid'])), color = "blue") +
			labs(list(title = "Residual vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())

	}

	if(result$reg_plots == "scatterlist") {
		plots <- list()
		for(i in reg_var2) {
			if('factor' %in% class(dat[,i])) {
				plots[[i]] <- ggplot(dat, aes_string(x=i, y=reg_var1, fill=i)) + geom_boxplot(alpha = .3)
			} else {
				p <- ggplot(dat, aes_string(x=i, y=reg_var1)) + geom_point()
        if(result$reg_line) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75,
				            linetype = "dashed", colour = 'black')
        if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
        plots[[i]] <- p
			}
		}
	}

	if(result$reg_plots == "resid_vs_predictorlist") {
		plots <- list()
		residuals <- mod$.resid
		rdat <- cbind(residuals,dat[,reg_var2])
		rdat <- data.frame(rdat)
		colnames(rdat) <- c('residuals',reg_var2)
		for(i in reg_var2) {
			if('factor' %in% class(dat[,i])) {
				plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_boxplot(fill = 'blue', alpha = .3)
			} else {
				p <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_point()
        if(result$reg_line) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75,
                                                 linetype = "dashed", colour = 'black')
        if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
        plots[[i]] <- p
			}
		}
	}

	if(result$reg_plots == "leverage_plots") {
		return(leveragePlots(result, main = "", ask=FALSE, id.n = 1, layout = c(ceiling(length(reg_var2)/2),2)))
	}

	# coefficient plots require the arm package (Gelman)
	# if(result$reg_plots == "coef") {
	# 	return(coefplot(result, xlab="", ylab="", main="Coefficient plot", col.pts="blue", CI=2))
	# }

	if(exists("plots"))
		suppressWarnings(suppressMessages(do.call(grid.arrange, c(plots, list(ncol = 2)))))
}

saveRegResiduals <- function(result = .regression()) {
	resid <- data.frame(result$residuals)
	changedata(resid, 'residuals')
}

# save residuals
observe({
	if(is.null(input$saveres) || input$saveres == 0) return()
	isolate({
		result <- .regression()
		if(is.character(result)) return()
		saveRegResiduals(result)
	})
})

reg_args <- as.list(formals(regression))

# list of function arguments
# reg_args$dataset <- "diamonds"
# reg_args$reg_var1 <- "price"
# reg_args$reg_var2 <- "carat"
# rm(r_env)
# result <- do.call(regression, reg_args)

# list of function inputs selected by user
reg_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(reg_args))
    reg_args[[i]] <- input[[i]]
  if(!input$show_filter) reg_args$data_filter = ""
  reg_args
})


.regression <- reactive({
	if(input$reg_var1 %>% not_available)
		return("This analysis requires a dependent variable of type integer\nor numeric and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("diamonds"))

	if(input$reg_var2 %>% not_available)
		return("Please select one or more independent variables.\n\n" %>% suggest_data("diamonds"))

	# result <- regression(input$dataset, input$reg_var1, input$reg_var2, input$reg_var3, input$reg_intsel,
	# 	input$reg_interactions, input$reg_predict, input$reg_predict_cmd, input$reg_predict_data, input$reg_standardize,
 #    input$reg_sumsquares, input$reg_confint, input$reg_conf_level, input$reg_rmse, input$reg_vif, input$reg_stepwise,
 #    input$reg_plots, input$reg_line, input$reg_loess)

	# result
	do.call(regression, reg_inputs())
})

observe({
  if(input$regressionReport %>% not_pressed) return()
  isolate({
		# inp <- list(input$dataset, input$reg_var1, input$reg_var2, input$reg_var3, input$reg_intsel,
		# 	input$reg_interactions, input$reg_predict, input$reg_predict_cmd, input$reg_predict_data,
  #     input$reg_standardize, input$reg_sumsquares, input$reg_confint, input$reg_conf_level, input$reg_rmse,
  #     input$reg_vif, input$reg_stepwise, input$reg_plots, input$reg_line,
  #     input$reg_loess)
		updateReport(reg_inputs() %>% clean_args, "regression",
		             round(7 * reg_plotWidth()/650,2), round(7 * reg_plotHeight()/650,2))
  })
})


################################################################
# Additional functions for regression
################################################################
reg_standardize_fun <- function(x) {
	if(is.numeric(x)) return(scale(x))
  x
}

reg_int_vec <- function(reg_vars, nway) {
	n <- length(reg_vars)
	iway <- c()
	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			iway <- c(iway, paste(reg_vars[i],reg_vars[j],sep=":"))
		}
	}
	if(n >= 3 && nway == '3way') {
		for(i in 1:(n-2)) {
			for(j in (i+1):(n-1)) {
				for(k in (j+1):n) {
					iway <- c(iway, paste(reg_vars[i],reg_vars[j],reg_vars[k],sep=":"))
				}
			}
		}
	}
	iway
}

vif_regression <- function(result = .regression()) {
	if(result$reg_vif) {
		if(length(result$reg_var2) > 1) {

  	 	VIF <- try(vif(result))
      if(is(VIF, 'try-error')) {
        cat("Insufficient number of independent variables selected to calculate VIF scores\n")
      } else {
	  	  cat("Variance Inflation Factors\n")
      	if(!is.null(dim(VIF))) VIF <- VIF[,'GVIF'] # needed when factors are included
        VIF <- data.frame("VIF" = VIF, "Rsq" = 1 - 1/VIF)
	  	  VIF <- VIF[order(VIF$VIF, decreasing=T),]
	  	  ifelse(nrow(VIF) < 8, return(VIF %>% t), return(VIF))
      }
		} else {
	  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
		}
	}
}

test_regression <- function(result = .regression()) {
	dat <- r_data[[result$dataset]]
	if(result$reg_standardize) dat <- data.frame(lapply(dat,reg_standardize_fun))

	sub_formula <- ". ~ 1"
	vars <- result$reg_var2

  if(result$reg_interactions != 'none' && !is.null(result$reg_intsel) && length(vars) > 1) {
		vars <- c(vars,result$reg_intsel)
	}

	not_selected <- setdiff(vars,result$reg_var3)
	if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))

	reg_sub <- update(result, sub_formula)
	anova(reg_sub, result, test='F')
}
