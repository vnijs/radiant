#' Conjoint analysis
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/conjoint.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param ca_dep_var The dependent variable (e.g., profile ratings)
#' @param ca_indep_var Independent variables in the regression
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param ca_rev Reverse the values of the dependent variable (`ca_dep_var`)
#'
#' @return A list with all variables defined in the function as an object of class conjoint
#'
#' @examples
#' result <- conjoint(dataset = "mp3", ca_dep_var = "Rating", ca_indep_var = "Memory:Shape")
#'
#' @seealso \code{\link{summary.conjoint}} to summarize results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @export
conjoint <- function(dataset, ca_dep_var, ca_indep_var,
                     data_filter = "",
                     ca_rev = FALSE) {

	dat <- getdata(dataset, c(ca_dep_var, ca_indep_var))

	# in case : was used to select a range of variables
  var_check(ca_indep_var, colnames(dat)[-1]) %>%
    { ca_indep_var <<- .$indep_var }

	formula <- paste(ca_dep_var, "~", paste(ca_indep_var, collapse = " + "))

	if(ca_rev) {
		ca_dep <- dat[,ca_dep_var]
		dat[,ca_dep_var] <- abs(ca_dep - max(ca_dep)) + 1
	}

	lm_mod <- lm(formula, data = dat)
	model <- lm_mod %>% tidy

	the_table <- ca_the_table(model, dat, ca_indep_var)

	environment() %>% as.list %>% set_class(c("conjoint",class(.)))
}

#' Summary method for conjoint output
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/conjoint.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{conjoint}}
#' @param ca_vif Shows multicollinearity diagnostics.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- conjoint(dataset = "mp3", ca_dep_var = "Rating", ca_indep_var = "Memory:Shape")
#' summary(result, ca_vif = TRUE)
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @importFrom car vif
#'
#' @export
summary.conjoint <- function(object,
                             ca_vif = FALSE,
                             ...) {

	cat("Conjoint analysis\n")
  cat("Data     :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
  cat("Dependent variable   :", object$ca_dep_var, "\n")
  cat("Independent variables:", paste0(object$ca_indep_var, collapse=", "), "\n\n")

	object$the_table %>%
	{
		cat("Conjoint part-worths:\n")
		print(.$PW, row.names = FALSE)
		cat("\nConjoint importance weights:\n")
		print(.$IW, row.names = FALSE)
	}

	cat("\nConjoint regression coefficients:\n")
  for(i in object$ca_indep_var) object$model$term %<>% gsub(i, paste0(i," > "), .)
	object$model$estimate %>% data.frame %>% round(3) %>%
	  set_colnames("coefficients") %>%
	  set_rownames(object$model$term) %>% print
	cat("\n")

	if(ca_vif) {

    if(length(object$ca_indep_var) > 1) {
      cat("Variance Inflation Factors\n")
      vif(object$lm_mod) %>%
        { if(!dim(.) %>% is.null) .[,"GVIF"] else . } %>% # needed when factors are included
        data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
        round(3) %>%
        .[order(.$VIF, decreasing=T),] %>%
        { if(nrow(.) < 8) t(.) else . } %>% print
    } else {
      cat("Insufficient number of attributes selected to calculate\nmulti-collinearity diagnostics")
    }
	}
}

#' Plot method for conjoint output
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/conjoint.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{conjoint}}
#' @param ca_plots Show either the part-worth ("pw") or importance-weights ("iw") plot
#' @param ca_scale_plot Scale the axes of the part-worth plots to the same range
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- conjoint(dataset = "mp3", ca_dep_var = "Rating", ca_indep_var = "Memory:Shape")
#' plot(result, ca_scale_plot = TRUE)
#' plot(result, ca_plots = "iw")
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{summary.conjoint}} to summarize results
#'
#' @export
plot.conjoint <- function(x,
                          ca_plots = "pw",
                          ca_scale_plot = FALSE,
                          ...) {

	object <- x; rm(x)

	the_table <- object$the_table
	plot_ylim <- the_table$plot_ylim

	if("pw" %in% ca_plots) {
		PW.df <- the_table[["PW"]]

		plots <- list()
		for(var in object$ca_indep_var) {
			PW.var <- PW.df[PW.df[,"Attributes"] == var,]

			# setting the levels in the same order as in the_table. Without this
			# ggplot would change the ordering of the price levels
			PW.var$Levels <- factor(PW.var$Levels,levels=PW.var$Levels,ordered=FALSE)

			p <- ggplot(PW.var, aes_string(x="Levels", y="PW", group = 1)) +
				  geom_line(colour="blue", linetype = 'dotdash', size=.7) +
	  		  geom_point(colour="blue", size=4, shape=21, fill="white") +
		  	  labs(list(title = paste("Part-worths for", var), x = ""))
		  	  # theme(axis.text.x = element_text(angle = 45, hjust = 1))

		  if(ca_scale_plot) p <- p + ylim(plot_ylim[var,"Min"],plot_ylim[var,"Max"])
			plots[[var]] <- p
		}
		do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2))))
	}

	if("iw" %in% ca_plots) {
		IW.df <- the_table[['IW']]
		p <- ggplot(IW.df, aes_string(x="Attributes", y="IW", fill = "Attributes")) + geom_bar(stat = "identity", alpha = .5) +
			theme(legend.position = "none") + labs(list(title = "Importance weights"))
		print(p)
	}
}


#' Function to calculate the PW and IW table for conjoint
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/conjoint.html} for an example in Radiant
#'
#' @param model Tidied model results (broom) output from \code{\link{conjoint}} passed on by summary.conjoint
#' @param dat Conjoint data
#' @param ca_indep_var Independent variables used in the conjoint regression
#'
#' @examples
#' result <- conjoint(dataset = "mp3", ca_dep_var = "Rating", ca_indep_var = "Memory:Shape")
#' ca_the_table(result$model, result$dat, result$ca_indep_var)
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{summary.conjoint}} to summarize results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @export
ca_the_table <- function(model, dat, ca_indep_var) {
	if(is.character(model)) return(list("PW" = "No attributes selected."))

	attr <- select_(dat, .dots = ca_indep_var)
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
