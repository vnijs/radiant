#' Create fractional factorial design for conjoint analysis
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/conjoint_profiles.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#'
#' @return A list with all variables defined in the function as an object of class conjoint_profiles
#'
#' @examples
#' ca_prof <- readLines(system.file("examples/profiles-movie.txt", package='radiant'))
#' result <- conjoint_profiles("ca_prof")
#'
#' @seealso \code{\link{summary.conjoint_profiles}} to summarize results
#'
#' @export
conjoint_profiles <- function(dataset) {

	ca_str <- getdata(dataset)
	cmd <- "ca_attr <- list(c()"
	for(l in ca_str) {
		if(l != "") cmd <- paste(cmd, ",", l)
	}
	cmd <- paste(cmd, ")")
	eval(parse(text = cmd))
	ca_attr <- ca_attr[-1]
	cn <- names(ca_attr)

	# reordering the attributes affects the number of profiles generated - strange
	for(itt in 1:20) {
		ret <- sample(ca_attr) %>%
						 ff_design(itt)
		if(!is.null(ret)) break
	}

	frac <- ret$frac[,cn] %>% arrange_(.dots = cn) %>%
					  data.frame(Profile = 1:nrow(.), .)

	prof_cor <- cor(data.matrix(frac[,-1]))

	full <- ret$full[,cn] %>% arrange_(.dots = cn) %>%
					  data.frame(Profile = 1:nrow(.), .)

	rm(l, itt, cmd, ret, cn)

	environment() %>% as.list %>% set_class(c("conjoint_profiles",class(.)))
}

#' Summary method for the conjoint_profiles function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/conjoint_profiles.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{conjoint_profiles}}
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' ca_prof <- readLines(system.file("examples/profiles-movie.txt", package='radiant'))
#' result <- conjoint_profiles("ca_prof")
#' summary(result)
#'
#' @seealso \code{\link{conjoint_profiles}} to calculate results
#'
#' @export
summary.conjoint_profiles <- function(object, ...) {

	cat("Generate conjoint profiles\n")
	cat("Data      :", object$dataset, "\n")
	cat("# profiles:", nrow(object$frac))

	cat("\n\nAttributes and levels:\n")
	cat(paste0(object$ca_str, collapse="\n"),"\n")

	cat("\nFractional factorial design correlations:\n")
	print(object$prof_cor, row.names = FALSE)

	cat("\nFractional factorial design:\n")
	print(object$frac, row.names = FALSE)

	cat("\nFull factorial design:\n")
	print(object$full, row.names = FALSE)
}

#' Function to generate a fractional factorial design
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/marketing/conjoint_profiles.html} for an example in Radiant
#'
#' @param attr Attributes used to generate profiles
#' @param trial Number of trials that have already been run
#' @param rseed Random seed to use
#'
#' @seealso \code{\link{conjoint_profiles}} to calculate results
#' @seealso \code{\link{summary.conjoint_profiles}} to summarize results
#'
#' @importFrom AlgDesign optFederov
#'
#' @export
ff_design <- function(attr,
                      trial = 0,
                      rseed = 172110) {

	experiment <- expand.grid(attr)

	###############################################
	# eliminate combinations from experiment
	# by removing then from the variable _experiment_
	# http://stackoverflow.com/questions/18459311/creating-a-fractional-factorial-design-in-r-without-prohibited-pairs?rq=1
	###############################################

	nr_levels <- sapply(attr, length) %>% sum
	min_profiles <- nr_levels - length(attr) + 1
	max_profiles <- nrow(experiment)

	for (i in min_profiles:max_profiles) {
		set.seed(rseed) 		# needs to be in the loop
		design <- AlgDesign::optFederov(data = experiment, nTrials=i, maxIteration=1000)
		cor_mat <- cor(data.matrix(design$design))
		# print(as.dist(cor_mat), digits = 1)
		# cat('\nD-efficiency:',design$Dea,'\n')
		# if(design$Dea == 1) break
		if(det(cor_mat)==1) break
	}

	nr_profiles <- design$design %>% nrow
	if(nr_profiles> 24) {
		if(trial < 20) {
			return() 	# try again
		} else {
			cat(paste("The number of profiles required to generate an orthogonal design is\ngreater than the recommended maximum of 24. Consider\nreducing the number of attributes and/or levels.\n"))
		}
	}
	list(frac = design$design, full = experiment)
}

#####################################################################
# Exprerimenting with the DoE package
#####################################################################

# library(DoE.base)
# # attr <- read.csv("~/Desktop/conjoint_profiles.csv") %>%
# attr <- structure(list(sight = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L,
# 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("Not Staggered",
# "Staggered"), class = "factor"), food = structure(c(1L, 1L, 1L,
# 2L, 2L, 2L, 3L, 3L, 3L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L), .Label = c("Gourmet food",
# "Hot dogs and popcorn", "No food"), class = "factor"), price = structure(c(1L,
# 1L, 2L, 2L, 2L, 3L, 1L, 3L, 3L, 2L, 3L, 3L, 1L, 1L, 3L, 1L, 2L,
# 2L), .Label = c("$10", "$13", "$16"), class = "factor"), comfort = structure(c(1L,
# 3L, 3L, 1L, 2L, 1L, 2L, 2L, 3L, 2L, 1L, 2L, 2L, 3L, 3L, 1L, 1L,
# 3L), .Label = c("Average cup", "Average no cup", "Large cup"), class = "factor"),
#     audio.visual = structure(c(1L, 3L, 2L, 2L, 3L, 1L, 3L, 1L,
#     2L, 1L, 3L, 2L, 2L, 1L, 3L, 2L, 3L, 1L), .Label = c("Large digital",
#     "Large plain", "Small plain"), class = "factor")), class = "data.frame", row.names = c(NA,
# -18L), .Names = c("sight", "food", "price", "comfort", "audio.visual")) %>%
#     select(sight, food, price, comfort, audio.visual) %>%
#     arrange(sight, food, price, comfort, audio.visual)

# lev <- list(price = c('$10','$13','$16'),
#             sight = c('Staggered','Not Staggered'),
#             comfort = c('Average no cup','Average cup','Large cup'),
#             audio.visual = c('Small plain','Large plain','Large digital'),
#             food = c('No food','Hot dogs and popcorn','Gourmet food'))

# to_pad <- sapply(lev, length)
# max(to_pad) == min(to_pad)
# for(i in 1:length(lev)) {
# 	lev[[i]] <- c(lev[[i]], rep("",max(to_pad)-length(lev[[i]])))
# }

# write.csv(lev, "~/Desktop/test.csv", row.names = FALSE)
# read.csv(test, colClasses = "character")

# oa.design(seed = 1234, factor.names = sample(lev), columns="min34") %>%
#   select(sight, food, price, comfort, audio.visual) %>%
#   arrange(sight, food, price, comfort, audio.visual)

# cor_mat <- cor(data.matrix(res))
# cor_mat
# cat(paste(nrow(res)," ", det(cor_mat)), "\n")

# library(DoE.base)
# library(dplyr)
# lev <- list(price = c('$10','$13','$16'),
#             sight = c('Staggered','Not Staggered'),
#             comfort = c('Average no cup','Average cup','Large cup'),
#             audio.visual = c('Small plain','Large plain','Large digital'),
#             food = c('No food','Hot dogs and popcorn','Gourmet food'))

# oa.design(seed = 1234, factor.names = sample(lev), columns="min34") %>%
#   select(sight, food, price, comfort, audio.visual) %>%
#   arrange(sight, food, price, comfort, audio.visual)

# cor_mat <- cor(data.matrix(res))
# cor_mat
# cat(paste(nrow(res)," ", det(cor_mat)), "\n")

