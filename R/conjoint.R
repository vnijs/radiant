################################################################
# Conjoint regression
################################################################
#' @export
conjoint <- function(dataset, ca_var1, ca_var2,
                     data_filter = "",
                     show_filter = FALSE,
                     ca_rev = FALSE,
                     ca_vif = FALSE,
                     ca_plots = "pw",
                     ca_scale_plot = FALSE) {

	dat <- getdata_exp(dataset, c(ca_var1, ca_var2))
	formula <- paste(ca_var1, "~", paste(ca_var2, collapse = " + "))

	if(ca_rev) {
		ca_dep <- dat[,ca_var1]
		dat[,ca_var1] <- abs(ca_dep - max(ca_dep)) + 1
	}

	model <- lm(formula, data = dat) %>% tidy

	nrVars <- length(ca_var2)
	plot_height <- plot_width <- 500
	if (ca_plots == 'pw') {
		plot_height <- 325 * (1 + floor((nrVars - 1) / 2))
		plot_width <- 325 * min(nrVars,2)
	}

	the_table <- ca_the_table(model, dat, ca_var2)

	environment() %>% as.list %>% set_class(c("conjoint",class(.)))
}

# dat <- mtcars
# dat$vs <- as.factor(mtcars$vs)
# dataset = "dat"
# ca_var1 = "mpg"
# ca_var2 = "vs"
# result <- conjoint(dataset, ca_var1, ca_var2)
# summary_conjoint(result)

#' @export
summary.conjoint <- function(result) {

	cat("Conjoint analysis\n")
  cat("Data     :", result$dataset, "\n")
	if(result$show_filter) {
		if(result$data_filter %>% gsub("\\s","",.) != "")
			cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
	}
  cat("Dependent variable   :", result$ca_var1, "\n")
  cat("Independent variables:", paste0(result$ca_var2, collapse=", "), "\n\n")

	result$the_table %>%
	{
		cat("Conjoint part-worths:\n")
		print(.$PW, row.names = FALSE)
		cat("\nConjoint importance weights:\n")
		print(.$IW, row.names = FALSE)
	}

	cat("\nConjoint regression coefficients:\n")
  for(i in result$ca_var2) result$model$term %<>% gsub(i, paste0(i," > "), .)
	result$model$estimate %>% data.frame %>% round(3) %>%
	  set_colnames("coefficients") %>%
	  set_rownames(result$model$term) %>% print
	cat("\n")

	if(result$ca_vif) {

    if(length(result$ca_var2) > 1) {
      cat("Variance Inflation Factors\n")
      vif(result$model) %>%
        { if(!dim(.) %>% is.null) .[,"GVIF"] else . } %>% # needed when factors are included
        data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
        round(3) %>%
        .[order(.$VIF, decreasing=T),] %>%
        { if(nrow(.) < 8) t(.) else . } %>% print
    } else {
      cat("Insufficient number of attributes selected to calculate\nmulti-collinearity diagnostics")
    }
	}

		# if(length(result$ca_var2) > 1) {
	 #  	cat("Variance Inflation Factors\n")
	 #  	# from vif_regression
	 #  	VIF <- vif(result)
	 #  	if(!is.null(dim(VIF))) VIF <- VIF[,'GVIF'] # needed when factors are included
	 #  	VIF <- sort(VIF, decreasing = TRUE)
	 #  	ifelse(length(VIF) < 8, return(VIF), return(data.frame(VIF)))
		# } else {
	 #  	cat("Insufficient number of attributes/levels selected to calculate VIF scores\n")
		# }
}

#' @export
plot.conjoint <- function(result) {

	the_table <- result$the_table
	plot_ylim <- the_table$plot_ylim

	if(result$ca_plots == 'pw') {
		PW.df <- the_table[['PW']]

		plots <- list()
		for(var in result$ca_var2) {
			PW.var <- PW.df[PW.df[,'Attributes'] == var,]

			# setting the levels in the same order as in the_table. Without this
			# ggplot would change the ordering of the price levels
			PW.var$Levels <- factor(PW.var$Levels,levels=PW.var$Levels,ordered=FALSE)

			p <- ggplot(PW.var, aes(x=Levels, y=PW, group = 1)) +
				  geom_line(colour="blue", linetype = 'dotdash', size=.7) +
	  		  geom_point(colour="blue", size=4, shape=21, fill="white") +
		  	  labs(list(title = paste("Part-worths for", var), x = ""))
		  	  # theme(axis.text.x = element_text(angle = 45, hjust = 1))

		  if(result$ca_scale_plot) p <- p + ylim(plot_ylim[var,'Min'],plot_ylim[var,'Max'])
			plots[[var]] <- p
		}
		do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2))))
	} else {

		IW.df <- the_table[['IW']]
		p <- ggplot(IW.df, aes(x=Attributes, y=IW, fill = Attributes)) + geom_bar(stat = 'identity', alpha = .5) +
			theme(legend.position = "none") + labs(list(title = "Importance weights"))
		print(p)
	}
}

#' @export
ca_the_table <- function(model, dat, ca_var2) {
	if(is.character(model)) return(list("PW" = "No attributes selected."))

	attr <- select_(dat, .dots = ca_var2)
	isFct <- sapply(attr, is.factor)
	if(sum(isFct) < ncol(attr)) return(list("PW" = "Only factors can be used.", "IW" = "Only factors can be used."))
	levs <- lapply(attr[,isFct, drop = FALSE],levels)
	vars <- colnames(attr)[isFct]

	nlevs <- sapply(levs,length)
	PW.df <- data.frame(rep(vars,nlevs), unlist(levs))
	colnames(PW.df) <- c("Attributes","Levels")
	PW.df$PW <- 0

	# Calculate PW and IW's when interactions are present
	# http://www.slideshare.net/SunnyBose/conjoint-analysis-12090511
	rownames(PW.df) <- paste(PW.df[,'Attributes'], PW.df[,'Levels'], sep = "")

	coeff <- model$estimate
	BW.reg <- list("Attributes" = "Base utility", "Levels" = "", "PW" = coeff[1])
	PW.df[model$term[-1],'PW'] <- coeff[-1]
	PW.df

	minPW <- PW.df[tapply(1:nrow(PW.df),PW.df$Attributes,function(i) i[which.min(PW.df$PW[i])]),]
	maxPW <- PW.df[tapply(1:nrow(PW.df),PW.df$Attributes,function(i) i[which.max(PW.df$PW[i])]),]
	rownames(minPW) <- minPW$Attributes
	rownames(maxPW) <- maxPW$Attributes

	rangePW <- data.frame(cbind(maxPW[vars,'PW'],minPW[vars,'PW']))
	rangePW$Range <- rangePW[,1] - rangePW[,2]
	colnames(rangePW) <- c("Max","Min","Range")
	rownames(rangePW) <- vars

	# for plot range if standardized
	maxlim <- rangePW[,'Max'] > abs(rangePW[,'Min'])
	maxrange <- max(rangePW[,'Range'])
	plot_ylim <- rangePW[,c('Min','Max')]

	plot_ylim[maxlim,'Max'] <- plot_ylim[maxlim,'Max'] + maxrange - rangePW$Range[maxlim]
	plot_ylim[!maxlim,'Min'] <- plot_ylim[!maxlim,'Min'] - (maxrange - rangePW$Range[!maxlim])
	plot_ylim <- plot_ylim * 1.01 		# expanded max to avoid hiding max points in plot

	IW <- data.frame(vars)
	IW$IW <- rangePW$Range / sum(rangePW$Range)
	colnames(IW) <- c("Attributes","IW")

	PW.df[,'Attributes'] <- as.character(PW.df[,'Attributes'])
	PW.df[,'Levels'] <- as.character(PW.df[,'Levels'])
	PW.df <- rbind(PW.df, c("Base utility","~",coeff[1]))
	PW.df[,'PW'] <- as.numeric(PW.df[,'PW'])

	PW.df[,'PW'] <- round(PW.df[,'PW'],3)
	IW[,'IW'] <- round(IW[,'IW'],3)

	list('PW' = PW.df, 'IW' = IW, 'plot_ylim' = plot_ylim)
}

# vif.conjoint <- function(result = .conjoint()) {
# 	if(result$ca_vif) {
# 		if(length(result$ca_var2) > 1) {
# 	  	cat("Variance Inflation Factors\n")

# 	  	# from vif_regression
# 	  	VIF <- vif(result)
# 	  	if(!is.null(dim(VIF))) VIF <- VIF[,'GVIF'] # needed when factors are included
# 	  	VIF <- sort(VIF, decreasing = TRUE)
# 	  	ifelse(length(VIF) < 8, return(VIF), return(data.frame(VIF)))
# 		} else {
# 	  	cat("Insufficient number of attributes/levels selected to calculate VIF scores\n")
# 		}
# 	}
# }

# code for 'exploded logistic regression' when ranking data is provided
# require(MASS)
# greenwind$rnk <- ordered(as.factor(19 - greenwind$rank))
# levels(greenwind$rnk)
# greenwind$rnk
# head(greenwind)
# greenwind$price <- factor(greenwind$price, levels = c("1.59","1.39","1.19"))
# greenwind$brand <- factor(greenwind$brand, levels = c("Glory","K2R","Bissell"))
# levels(greenwind$brand)
# greenwind$brand

# m <- polr(rnk ~ design + brand + price + ghks + mbg, data = greenwind, Hess = TRUE)
# summary(m)
# cf <- coef(m)
# cf <- cf / max(abs(cf))
# cf

# greenwind$rnk_lm <- 19 - greenwind$rank
# l <- lm(rnk_lm ~ design + brand + price + ghks + mbg, data = greenwind)
# summary(l)
# cl <- coef(l)
# cl <- cl / max(abs(cl))
# cl

# df <- data.frame('cf' = cf, 'cl' = cl[-1])
# print(round(df,1), digits = 1)
