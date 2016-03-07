#' Create (partial) factorial design
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/doe.html} for an example in Radiant
#'
#' @param factors Categorical variables used as input for design
#' @param int Vector of interaction terms to consider when generating design
#' @param trials Number of trial to create. If NA then all feasible designs will be considered until a design with perfect D-efficiency is found
#' @param seed Random seed to use as the starting point
#'
#' @return A list with all variables defined in the function as an object of class conjoint_profiles
#'
#' @examples
#' "price; $10; $13; $16\nfood; popcorn; gourmet; no food" %>% doe
#'
#' @seealso \code{\link{summary.conjoint_profiles}} to summarize results
#'
#' @importFrom AlgDesign optFederov
#'
#' @export
doe <- function(factors, int = "", trials = NA, seed = NA) {
  df_list <-
    gsub("/","",factors) %>%
    gsub("\\\\n","\n",.) %>%
    gsub("[\n]{2,}$","",.) %>%
    strsplit(.,"\n") %>%
    .[[1]] %>% strsplit(";")

  df_names <- c()
  for (i in seq_len(length(df_list))) {
    dt <- df_list[[i]] %>% gsub("^\\s+|\\s+$", "", .)
    df_names <- c(df_names, dt[1])
    df_list[[i]] <- dt[-1]
  }
  names(df_list) <- df_names
  model <- paste0("~ ", paste0(df_names, collapse = " + "))
  nInt <- 0
  if (!is_empty(int)) {
    model <- paste0(model, " + ", paste0(int, collapse = " + "))
    nInt <- length(int)
  }

  part_frac <- function(df, model = ~ ., int = 0, trials = NA, seed = 172110) {

	  full <- expand.grid(df)

	  ###############################################
	  # eliminate combinations from full
	  # by removing then from the variable _experiment_
	  # http://stackoverflow.com/questions/18459311/creating-a-fractional-factorial-design-in-r-without-prohibited-pairs?rq=1
	  ###############################################

	  levs <- sapply(df, length)
	  nr_levels <- sum(levs)
	  min_trials <- nr_levels - length(df) + 1
	  max_trials <- nrow(full)

	  if (!is.null(trials) && !is.na(trials)) max_trials <- min_trials <- trials

	  eff <-
	    data.frame(
	      Trials = min_trials:max_trials,
	      "D-efficiency" = NA,
	      "Determinant" = NA,
	      "Balanced" = NA,
	      check.names = FALSE
	    )


	  for (i in min_trials:max_trials) {
	    if (!is.null(seed) && !is.na(seed)) set.seed(seed) # needs to be in the loop
	    design <- try(optFederov(model, data = full, nRepeats = 50,
                    nTrials = i, maxIteration=1000,
                    approximate = FALSE), silent = TRUE)

	    if (is(design, 'try-error')) next
	    cor_mat <- cor(data.matrix(design$design))
	    detcm <- det(cor_mat)
	    ind <- which(eff$Trials %in% i)
	    eff[ind,"D-efficiency"] <- design$Dea
	    eff[ind,"Determinant"] <- round(detcm,3)
	    eff[ind,"Balanced"] <-  all(i %% levs == 0)

	    if (design$Dea == 1) break
	  }

	  if (exists("cor_mat")) {
	    list(df = df, cor_mat = cor_mat, detcm = detcm, Dea = design$Dea,
	         part = arrange_(design$design, .dots = names(df)),
	         full = arrange_(full, .dots = names(df)),
	         eff = na.omit(eff),
	         seed = seed)
	  } else if (!is.na(trials)) {
	    "No solution exists for the selected number of trials"
	  } else {
	    "No solution found"
	  }
	}

  part_frac(df_list, model = as.formula(model), int = nInt, trials = trials, seed = seed) %>%
    set_class(c("doe",class(.)))
}

#' Summary method for doe function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/doe.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{conjoint_profiles}}
#' @param eff If TRUE print efficiency output
#' @param part If TRUE print partial factorial
#' @param full If TRUE print full factorial
#' @param ... further arguments passed to or from other methods.
#'
#' @seealso \code{\link{doe}} to calculate results
#'
#' @examples
#' "price; $10; $13; $16\nfood; popcorn; gourmet; no food" %>% doe %>% summary
#'
#' @export
summary.doe <- function(object, eff = TRUE, part = TRUE, full = TRUE, ...) {

  if (!is.list(object)) return(object)

  cat("Experimental design\n")
  cat("# trials for partial factorial:", nrow(object$part),"\n")
  cat("# trials for full factorial   :", nrow(object$full),"\n")
  if (!is.null(object$seed) && !is.na(object$seed))
    cat("Random seed                   :", object$seed,"\n")

  cat("\nAttributes and levels:\n")
  nl <- names(object$df)
  for (i in nl) {
    cat(paste0(i, ":"), paste0(object$df[[i]], collapse = ", "), "\n")
  }

  if (eff) {
    cat("\nDesign efficiency:\n")
    print(dfprint(object$eff, dec = 3), row.names = FALSE)
  }

  if (part) {
    cat("\nPartial factorial design correlations:\n")
    nrdec <- ifelse (object$detcm == 1, 0, 3)
    print(dfprint(data.frame(object$cor_mat), dec = nrdec) , row.names = FALSE)

    cat("\nPartial factorial design:\n")
    print(object$part)
  }

  if (full) {
    cat("\nFull factorial design:\n")
    print(object$full)
  }
}
