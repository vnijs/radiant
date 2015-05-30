#' Attribute based brand maps
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/pmap.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param pm_brand A character variable with brand names
#' @param pm_attr Names of numeric variables
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param pm_pref Names of numeric brand preference measures
#' @param pm_dim_number Number of dimensions
#'
#' @return A list of all variables defined in the function as an object of class pmap
#'
#' @examples
#' result <- pmap("computer","Brand","HighEnd:Business")
#'
#' @seealso \code{\link{summary.pmap}} to summarize results
#' @seealso \code{\link{plot.pmap}} to plot results
#'
#' @importFrom psych principal
#' @import GPArotation
#'
#' @export
pmap <- function(dataset, pm_brand, pm_attr,
                 data_filter = "",
                 pm_pref = "",
                 pm_dim_number = 2) {

	pm_dim_number <- as.numeric(pm_dim_number)
	dat <- getdata(dataset, c(pm_brand, pm_attr), filt = data_filter)

	brands <- dat[,1] %>% as.character %>% gsub("^\\s+|\\s+$", "", .)
	f_data <- dat[,-1]
	nrObs <- nrow(dat)

	# in case : is used
	if (length(pm_attr) < ncol(f_data)) pm_attr <- colnames(f_data)

	fres <- sshhr( psych::principal(cov(f_data), nfactors=pm_dim_number,
	               rotate='varimax', scores=FALSE, oblique.scores=FALSE) )

	m <- as.data.frame(fres$loadings[,colnames(fres$loadings)]) %>% as.matrix
	cscm <- m %*% solve(crossprod(m))
	# store in fres so you can re-use save_factors
	fres$scores <- scale(as.matrix(f_data), center = TRUE, scale = TRUE) %*% cscm
	rownames(fres$scores) <- brands

	if (!is.null(pm_pref) && pm_pref != "") {
		pref_cor <- getdata(dataset, pm_pref, filt = data_filter) %>%
								  cor(fres$scores) %>%
								  data.frame
		pref_cor$communalities <- rowSums(pref_cor^2)
	}

	rm(dat, f_data, m, cscm)

	if (!is_string(dataset)) dataset <- "-----"

	environment() %>% as.list %>% set_class(c("pmap",class(.)))
}

#' Summary method for the pmap function
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/pmap.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{pmap}}
#' @param pm_cutoff Show only loadings with (absolute) values above pm_cutoff (default = 0)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- pmap("computer","Brand","HighEnd:Business")
#' summary(result)
#' summary(result, pm_cutoff = .3)
#' result <- pmap("computer","Brand","HighEnd:Dated", pm_pref = c("Innovative","Business"))
#' summary(result)
#' computer %>% pmap("Brand","HighEnd:Dated", pm_pref = c("Innovative","Business")) %>%
#'   summary
#'
#' @seealso \code{\link{pmap}} to calculate results
#' @seealso \code{\link{plot.pmap}} to plot results
#'
#' @export
summary.pmap <- function(object,
                         pm_cutoff = 0,
                         ...) {

 	cat("Attribute based brand map\n")
	cat("Data        :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Attributes  :", paste0(object$pm_attr, collapse=", "), "\n")
	if (!is.null(object$pm_pref) && object$pm_pref != "")
		cat("Preferences :", paste0(object$pm_pref, collapse=", "), "\n")
	cat("# dimensions:", object$pm_dim_number, "\n")
	cat("Rotation    : varimax\n")
	cat("Observations:", object$nrObs, "\n")

	cat("\nBrand - Factor scores:\n")
	round(object$fres$scores,2) %>% print

	cat("\nAttribute - Factor loadings:\n")
	# convert loadings object to data.frame
	lds <- object$fres$loadings
	dn <- dimnames(lds)
	lds %<>% matrix(nrow = length(dn[[1]])) %>%
		set_colnames(dn[[2]]) %>% set_rownames(dn[[1]]) %>%
		data.frame

	# show only the loadings > ff_cutoff
  ind <- abs(lds) < pm_cutoff
  print_lds <- round(lds,2)
  print_lds[ind] <- ""
  print(print_lds)

	if (!is.null(object$pm_pref) && object$pm_pref != "") {
		cat("\nPreference correlations:\n")
		print(round(object$pref_cor,2), digits = 2)
	}

  # fit measures
	cat("\nFit measures:\n")
	colSums(lds^2) %>%
		rbind(., . / length(dn[[1]])) %>%
		rbind(., cumsum(.[2,])) %>%
		round(2) %>%
		set_rownames(c("Eigenvalues","Variance %","Cumulative %")) %>%
		print

	cat("\nAttribute communalities:")
	data.frame(1 - object$fres$uniqueness) %>%
		set_colnames("") %>% round(2) %>%
		print
}

#' Plot method for the pmap function
#'
#' @details See \url{http://vnijs.github.io/radiant/marketing/pmap.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{pmap}}
#' @param pm_plot Components to include in the plot ("brand", "attr"). If data on preferences is available use "pref" to add preference arrows to the plot
#' @param pm_scaling Arrow scaling in the brand map
#' @param pm_fontsz Font size to use in plots
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- pmap("computer","Brand","HighEnd:Business")
#' plot(result, pm_plot = "brand")
#' plot(result, pm_plot = c("brand","attr"))
#' plot(result, pm_plot = c("brand","attr"))
#' plot(result, pm_scaling = 1, pm_plot = c("brand","attr"))
#' result <- pmap("computer","Brand","HighEnd:Dated",
#'               pm_pref = c("Innovative","Business"))
#' plot(result, pm_plot = c("brand","attr","pref"))
#'
#' @seealso \code{\link{pmap}} to calculate results
#' @seealso \code{\link{summary.pmap}} to plot results
#'
#' @importFrom wordcloud textplot
#'
#' @export
plot.pmap <- function(x,
                      pm_plot = "",
                      pm_scaling = 2.1,
                      pm_fontsz = 1.3,
                      ...) {

	pm_scaling <- as.numeric(pm_scaling)
	object <- x; rm(x)

	std_pc <- pm_scaling * object$pref_cor
	std_m <- pm_scaling * object$fres$loadings
	std_scores <- object$fres$scores
	lab_buf <- 1.1

	# adding a buffer so the labels don't move off the screen
	lim <- max(abs(std_m),abs(std_scores)) * lab_buf

	# using ggplot is not an option at this time because labels are likely to overlap
	# the wordcloud with wordlayout package may be an option but it does not seem to produce the
	# desired effect
	# wctemp <- wordcloud::wordlayout(mtcars$wt, mtcars$mpg, rownames(mtcars), cex = 3)[,1:2] %>%
	# 						data.frame %>%
	# 					  set_colnames(c("wt","mpg"))
	# use geom_text and geom_points
	# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_segment
	# http://docs.ggplot2.org/0.9.3.1/geom_abline.html

	if (object$pm_dim_number == 3) {
		op <- par(mfrow=c(3,1))
		pm_fontsz <- pm_fontsz + .6
	} else {
		op <- par(mfrow=c(1,1))
	}

	for (i in 1:(object$pm_dim_number-1)) {
		for (j in (i+1):object$pm_dim_number) {

			plot(c(-lim,lim),type = "n",xlab='', ylab='', axes = F, asp = 1,
			     yaxt = 'n', xaxt = 'n', ylim=c(-lim, lim), xlim=c(-lim,lim))
			title(paste("Dimension",i,"vs Dimension",j), cex.main = pm_fontsz)
			abline(v=0, h=0)

			if ("brand" %in% pm_plot) {
				points(std_scores[,i], std_scores[,j], pch = 16, cex = .6)
				wordcloud::textplot(std_scores[,i], std_scores[,j]+(.04*lim),
				                    object$brand, cex = pm_fontsz, new = FALSE)
			}

			if ("attr" %in% pm_plot) {
				wordcloud::textplot(std_m[,i]*lab_buf, std_m[,j]*lab_buf,
				                    object$pm_attr, cex = pm_fontsz,
				                    col = "darkblue", new = FALSE)
				# add arrows
				for (k in object$pm_attr)
					arrows(0,0, x1=std_m[k,i], y1=std_m[k,j], lty='dashed', length=.05)
			}

			if ("pref" %in% pm_plot) {
				if (nrow(std_pc) > 1) {
					# textplot needs at least two coordinates
					wordcloud::textplot(std_pc[,i]*lab_buf, std_pc[,j]*lab_buf,
					                    object$pm_pref, cex = pm_fontsz,
					                    col="darkred", new = FALSE)
				} else {
					text(std_pc[,i]*lab_buf, std_pc[,j]*lab_buf, object$pm_pref,
					     cex = pm_fontsz, col="darkred")
				}
				for (l in object$pm_pref) {
					arrows(0,0, x1=std_pc[l,i], y1=std_pc[l,j], lty='dashed', col="red",
					       length=.05)
				}
			}
		}
	}

	par(op)
}
