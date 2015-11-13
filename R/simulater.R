### Make another tab where you can run multiple sims (e.g., grid-search) and
### aggregate the results
### This would also be useful for Conjoint I think

#' Simulate data for decision analysis
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/simulater.html} for an example in Radiant
#'
#' @param const A string listing the constants to include in the analysis (e.g., "cost = 3; size = 4")
#' @param norm A string listing the normally distributed random variables to include in the analysis (e.g., "demand 2000 1000" where the first number is the mean and the second is the standard deviation)
#' @param unif A string listing the uniformly distributed random variables to include in the analysis (e.g., "demand 0 1" where the first number is the minimum value and the second is the maximum value)
#' @param discrete A string listing the random variables with a discrete distribution to include in the analysis (e.g., "price 5 .3 8 .7" where for each pair of numbers the first is the value and the second the probability
#' @param binom A string listing the random variables with a binomail distribution to include in the analysis (e.g., "crash 100 .01") where the first number is the number of trials and the second is the probability of success)
#' @param sequ A string listing the start and end for a sequence to include in the analysis (e.g., "trend 1 100 1"). The number of 'steps' is determined by the number of simulations.
#' @param form A string with the formula to evaluate (e.g., "profit = demand * (price - cost)")
#' @param seed To repeat a simulation with the same randomly generated values enter a number into Random seed input box.
#' @param name To save the simulated data for further analysis specify a name in the Sim name input box. You can then investigate the simulated data by choosing the specified name from the Datasets dropdown in any of the other Data tabs.
#' @param nr Number of simulation runs
#' @param dat Data list from previous simulation. Used by repeater function
#'
#' @return A data.frame with the created variables
#'
#' @examples
#' result <- simulater(const = "cost 3", norm = "demand 2000 1000",
#'                     discrete = "price 5 .3 8 .7",
#'                     form = "profit = demand * (price - cost)")
#'
#' @seealso \code{\link{summary.simulater}} to summarize results
#' @seealso \code{\link{plot.simulater}} to plot results
#' @export
simulater <- function(const = "",
                      norm = "",
                      unif = "",
                      discrete = "",
                      binom = "",
                      sequ = "",
                      form = "",
                      seed = "",
                      name = "",
                      nr = 1000,
                      dat = NULL) {

  # rm(list = ls())
  # const <- "non_labor_cost 3995; cost 11"
  # unif <- "labor_cost 5040 6860"
  # discrete <- "price 20 .25 18.5 .35 16.5 .3 15 .1"
  # norm <- "nr_meals 3000 1000"
  # form <- "profit = (price - cost)*nr_meals - labor_cost - non_labor_cost"
  # seed <- "1234"
  # name <- ""
  # nr <- 1000
  # const = "var_cost 5;fixed_cost 1000"
  # norm = "demand 1000 100"
  # discrete = "price 6 .30 8 .70"
  # form = "profit = demand*(price - var_cost) - fixed_cost"
  # const = "var_cost 5;fixed_cost 1000"
  # norm = "demand 1000 100"
  # discrete = "price 6 .30 8 .70"
  # form = "demand = demand -50*price;profit = demand*(price-var_cost) - fixed_cost"

  # const = "cost 1.25;salvage .5;price 5;q .5:100"
  # norm = "demand 535.74 145"
  # form = "profit = -cost*q + 5*pmin(q,demand) + .5 * pmax(0, q - demand)"
  # seed = "1234"
  # name = "sim1"
  # nr = 1000

  ## remove any non-numbers from seed, including spaces
  seed %>% gsub("[^0-9]","",.) %>% { if (. != "") set.seed(seed) }

  if (is.null(dat)) dat <- list()

  cleaner <- function(x) x %>% gsub("[ ]{2,}"," ",.) %>%
    gsub("[ ]*[\n;]+[ ]*",";",.) %>%
    gsub("[;]{2,}",";",.) %>%
    gsub(";$","",.)

  spliter <- function(x, symbol = " ") x %>% strsplit(., ";") %>% extract2(1) %>% strsplit(.,symbol)

  ## parsing constant
  # const <- "non_labor_cost 3995;\n;\n;cost 11\n   \n \n\n\n\n\n     \n    \n   "
  const %<>% cleaner
  if (const != "") {
    s <- const %>% spliter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- as.numeric(.[2]) %>% rep(,nr) }
  }

  ## parsing uniform
  unif %<>% cleaner
  if (unif != "") {
    s <- unif %>% spliter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- runif(nr, as.numeric(.[2]) , as.numeric(.[3]))}
  }

  ## parsing normal
  norm %<>% cleaner
  if (norm != "") {
    s <- norm %>% spliter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- rnorm(nr, as.numeric(.[2]) , as.numeric(.[3]))}
  }

  ## parsing binomial
  binom %<>% cleaner
  if (binom != "") {
    s <- binom %>% spliter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- rbinom(nr, as.numeric(.[2]) , as.numeric(.[3]))}
  }

  ## parsing sequence
  sequ %<>% cleaner
  # print(sequ)
  if (sequ != "") {
    s <- sequ %>% spliter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- seq(as.numeric(.[2]) , as.numeric(.[3]), length.out = as.numeric(nr))}
  }

  ## parsing discrete
  # discrete = "price 6 .30 8 .70"
  discrete %<>% cleaner
  if (discrete != "") {
    s <- discrete %>% spliter
    for (i in 1:length(s)) {
      par <- s[[i]][-1] %>% as.numeric %>% matrix(ncol = 2)
      if (sum(par[,2]) != 1) {
        mess <- c("error",paste0("Probabilities for discrete variable do not sum to 1"))
        return(mess %>% set_class(c("simulater", class(.))))
      }
      dat[[s[[i]][1]]] <- sample(par[,1], nr, replace = TRUE, prob = par[,2])

      # par <- s[[i]][-1] %>% as.numeric %>% matrix(nrow = 2)
      # if (sum(par[2,]) != 1) message("Probabilities for discrete variable do not sum to 1!")
      # dat[[s[[i]][1]]] <- sample(par[1,], nr, replace = TRUE, prob = par[2,])
    }
  }

  ## parsing formula
  # form <- "profit = (price - cost)*nr_meals - labor_cost - non_labor_cost\n ; \n margin = price - cost ;;;   ; \n  \n  "
  # form = "demand = demand -50*price;profit = demand*(price-var_cost) - fixed_cost"
  # form = "demand = demand - .1*lag(demand, 0);profit = demand*(price-var_cost) - fixed_cost"
  # form = "demand = demand - .1*lag(demand, default=0);# profit = demand*(price-var_cost) - fixed_cost"
  # form = "#demand = demand - .1*lag(demand, default=0);# profit = demand*(price-var_cost) - fixed_cost"
  # library(magrittr)
  # form %>% gsub(";\\s*\\#.*$","",.)
  # form %>% gsub("^\\s*#.*[\n;]","",.)
  # form %>% gsub(";\\s*\\#.*$","",.) %>%
  #   gsub(";\\s*\\#.*;","",.)

  # gsub(";\\s*\\#.*$","",form, perl = TRUE)
  # gsub("[;\\^]\\s*#.*[\n;$]","",form)
  # gsub(";\\s*#.*[\n|;|$]","",form)
  # gsub(";\\s*#.*[\\n$]","",form)
  # gsub(";\\s*\\#.*[\\n;$]","",form, perl = TRUE)
  # gsub(";#.*$","",form)
  # gsub(";#.*[;$]","",form)

  form %<>% cleaner
  if (form != "") {
    s <- form %>% gsub(" ","",.) %>% spliter("=")
    for (i in 1:length(s)) {
      if (grepl("^#",s[[i]][1])) next
      obj <- s[[i]][1]
      fobj <- s[[i]][-1]
      if (length(fobj) > 1) fobj <- paste0(fobj, collapse = "=")
      out <- try(do.call(with, list(dat, parse(text = fobj))), silent = TRUE)
      if (!is(out, 'try-error')) {
        dat[[obj]] <- out
      } else {
        dat[[obj]] <- NA
        mess <- c("error",paste0("Formula was not successfully evaluated:\n\n", strsplit(form,";") %>% unlist %>% paste0(collapse="\n"),"\n\nMessage: ", attr(out,"condition")$message))
        return(mess %>% set_class(c("simulater", class(.))))
      }
    }
  }

  ret <- list(dat = dat %>% as.data.frame, sim_call = as.list(match.call())[-1]) %>%
    set_class(c("simulater", class(.)))

  name %<>% gsub(" ","",.)
  if (name != "") {
    if (exists("r_env")) {
      env <- r_env
    } else if (exists("r_data")) {
      env <- pryr::where("r_data")
    } else {
      return(ret)
    }

    mess <- paste0("\n### Simulated data\n\nFormula:\n\n",
                   gsub("*","\\*",form, fixed = TRUE) %>% gsub(";","\n\n", .), "\n\nDate: ",
                   lubridate::now())
    env$r_data[[name]] <- ret$dat
    env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique
    env$r_data[[paste0(name,"_descr")]] <- mess

    sim_list <- paste0(name,"_list")
    env$r_data[[sim_list]] <- ret

    return(sim_list %>% set_class(c("simulater", class(.))))
  }

  ret
}

#' Summary method for the simulater function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/simulater.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{simulater}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- simulater(norm = "demand 2000 1000")
#' summary(result)
#'
#' @seealso \code{\link{simulater}} to generate the results
#' @seealso \code{\link{plot.simulater}} to plot results
#'
#' @export
summary.simulater <- function(object, ...) {

  if (is.character(object)) {
    if (object[1] == "error") return(cat(object[2]))
    else object <- getdata(object)
  }

  cat("Simulation\n")
  cat("Simulations:", object$sim_call$nr, "\n")
  cat("Random seed:", object$sim_call$seed, "\n")
  cat("Sim data   :", object$sim_call$name, "\n")
  if (!is_empty(object$sim_call$const))
    cat("Constant   :", gsub(";", "; ", object$sim_call$const) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$binom))
    cat("Binomial   :", gsub(";", "; ", object$sim_call$binom) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$discrete))
    cat("Discrete   :", gsub(";", "; ", object$sim_call$discrete) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$norm))
    cat("Normal     :", gsub(";", "; ", object$sim_call$norm) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$unif))
    cat("Uniform    :", gsub(";", "; ", object$sim_call$unif) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$form))
    cat(paste0("Formulas   :\n\t", object$sim_call$form %>% gsub(";","\n",.) %>% gsub("\n","\n\t",.), "\n"))

    # cat(paste0("Formulas   :\n\t", object$sim_call$form %>% gsub("[^;]\n","\n\t",.) %>% gsub(";[^\n]","\n\t",.) %>% gsub(";\n", "\n\t",.),"\n"))
    # cat(paste0("Formulas   :\n\t", object$sim_call$form %>% gsub("[^;]\n","\n\t",.) %>% gsub(";[^\n]", "\n\t",.)))
    # cat(paste0("Formulas   :\n\t", gsub("[\n;]","\n\t",object$sim_call$form), "\n"))
    # cat(paste0("Formulas   :\n\t", gsub("\n;*","\n\t",object$sim_call$form), "\n"))
  cat("\n")

  sim_summary(object$dat)
}

#' Plot method for the simulater function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/simulater} for an example in Radiant
#'
#' @param x Return value from \code{\link{simulater}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- simulater(const = "cost 3", norm = "demand 2000 1000",
#'                     discrete = "price 5 .3 8 .7",
#'                     form = "profit = demand * (price - cost)")
#' plot(result)
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{summary.single_mean}} to summarize results
#'
#' @export
plot.simulater <- function(x, shiny = FALSE, ...) {

  if (is.character(x)) {
    if (x[1] == "error") return(invisible())
    object <- getdata(x)$dat
    if (nrow(object) == 0) return(invisible())
  } else {
    object <- x$dat
  }
  rm(x)

  plot_list <- list()
  for (i in colnames(object)) {
    dat <- select_(object, .dots = i)
    if (!does_vary(object[[i]])) next
    plot_list[[i]] <-
      visualize(select_(object, .dots = i), xvar = i, bins = 20, custom = TRUE)
  }

  sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = min(length(plot_list),2)))) ) %>%
    { if (shiny) . else print(.) }
}

#' Repeat simulation
#'
#' @param nr Number times to repeat the simulation
#' @param vars Variables to use in repeated simulation
#' @param grid Expression to use in grid search for constants
#' @param seed To repeat a simulation with the same randomly generated values enter a number into Random seed input box.
#' @param name To save the simulated data for further analysis specify a name in the Sim name input box. You can then investigate the simulated data by choosing the specified name from the Datasets dropdown in any of the other Data tabs.
#' @param sim Return value from the simulater function
#'
#' @examples
#' result <- simulater(const = "cost 3", norm = "demand 2000 1000",
#'                     discrete = "price 5 .3 8 .7",
#'                     form = "profit = demand * (price - cost)")
#'
#' repeater(sim = result)
#'
#'
#' @export
repeater <- function(nr = 12,
                     vars = "",
                     grid = "",
                     seed = "",
                     name = "",
                     sim = "") {

  if (is.character(sim)) sim <- getdata(sim)

  seed %>% gsub("[^0-9]","",.) %>% { if (. != "") set.seed(seed) }
  if (identical(vars, "")) return()

  ## from http://stackoverflow.com/a/7664655/1974918
  ## keep those list elements that, e.g., q is in
  nr_sim <- nrow(sim$dat)
  sc <- sim$sim_call
  sc$name <- sc$seed <- "" ## cleaning up the sim call
  sc_keep <- grep(paste(vars, collapse = "|"), sc, value=TRUE)
  sc[1:which(names(sc) == "form")] <- ""
  sc[names(sc_keep)] <- sc_keep
  sc$dat <- sim$dat %>% as.list
  sc$form

  rep_sim <- function(run_nr) {
    bind_cols(
      data_frame(run = rep(run_nr, nr_sim), sim = 1:nr_sim),
      do.call(simulater, sc)$dat
    )
  }

  rep_grid_sim <- function(gval) {
    sc_grid <- grep(paste(vars, collapse = "|"), sc_keep, value=TRUE) %>% {.[which(names(.) != "form")]}
    sc[names(sc_grid)] <- sub(paste0("[;\n]", vars, " [.0-9]+"), paste0("\n", vars, " ", gval), sc_grid) %>%
                          sub(paste0("^", vars, " [.0-9]+"), paste0(vars, " ", gval), .)

    bind_cols(
      data_frame(run = rep(gval, nr_sim), sim = 1:nr_sim),
      do.call(simulater, sc)$dat
    )
  }

  if (grid == "") {
    ret <- bind_rows(lapply(1:nr, rep_sim)) %>% set_class(c("repeater", class(.)))
  } else {
    grid %<>% gsub("\"","\'", .) %>% gsub(";",",", .)
    grid <- try(eval(parse(text = paste0("with(sim$dat, expand.grid(", grid ,"))"))), silent = TRUE)
    ret <- bind_rows(lapply(grid[,1], rep_grid_sim)) %>% set_class(c("repeater", class(.)))
  }

  name %<>% gsub(" ","",.)
  if (name != "") {
    if (exists("r_env")) {
      env <- r_env
    } else if (exists("r_data")) {
      env <- pryr::where("r_data")
    } else {
      return(ret)
    }

    mess <- paste0("\n### Repeated simulation data\n\nFormula:\n\n",
                   gsub("*","\\*",sc$form, fixed = TRUE) %>% gsub(";","\n\n", .), "\n\nDate: ",
                   lubridate::now())

    env$r_data[[name]] <- ret
    env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique
    env$r_data[[paste0(name,"_descr")]] <- mess
    return(name %>% set_class(c("repeater", class(.))))
  }

  ret
}

#' Summarize repeated simulation
#'
#' @param object Return value from \code{\link{repeater}}
#' @param sum_vars (Numerical) variables to summaries
#' @param byvar Variable(s) to group data by before summarizing
#' @param fun Functions to use for summarizing
#' @param form A string with the formula to evaluate (e.g., "profit = demand * (price - cost)")
#' @param name To save the simulated data for further analysis specify a name in the Sim name input box. You can then investigate the simulated data by choosing the specified name from the Datasets dropdown in any of the other Data tabs.
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.repeater <- function(object,
                             sum_vars = "",
                             byvar = "",
                             fun = "sum_rm",
                             form = "",
                             name = "",
                             ...) {

  if (identical(sum_vars, "")) return("Select one or more 'Output variables'")
  if (is.character(object)) object <- getdata(object)

  sim <- max(object$sim)
  runs <- length(unique(object$run))

  ## show results
  cat("Repeated simulation\n")
  cat("Simulations :", sim, "\n")
  cat("Runs        :", runs, "\n")
  cat("Group by    :", byvar, "\n")
  cfun <- sub("_rm$","",fun)
  cat("Function    :", cfun, "\n")
  # cat("Random  seed:", object$sim_call$seed, "\n")
  cat("Repeat  data:", name, "\n")
  cat("Summary data:", paste0(name,"_",cfun) , "\n")

  object %<>% group_by_(byvar) %>%
    summarise_each_(make_funs(fun), vars = sum_vars) %>%
    select(-1)

  if (length(sum_vars) == 1 && length(fun) > 1) colnames(object) <- paste0(sum_vars, "_", colnames(object))

  cleaner <- function(x) x %>% gsub("[ ]{2,}"," ",.) %>%
    gsub("[ ]*[\n;]+[ ]*",";",.) %>%
    gsub("[;]{2,}",";",.) %>%
    gsub(";$","",.)

  spliter <- function(x, symbol = " ") x %>% strsplit(., ";") %>% extract2(1) %>% strsplit(.,symbol)

  form %<>% cleaner
  if (form != "") {
    s <- form %>% gsub(" ","",.) %>% spliter("=")
    for (i in 1:length(s)) {
      obj <- s[[i]][1]
      fobj <- s[[i]][-1]
      if (length(fobj) > 1) fobj <- paste0(fobj, collapse = "=")
      out <- try(do.call(with, list(object, parse(text = fobj))), silent = TRUE)
      if (!is(out, 'try-error')) {
        object[[obj]] <- out
      } else {
        object[[obj]] <- NA
        mess <- paste0("Formula was not successfully evaluated:\n\n", strsplit(form,";") %>% unlist %>% paste0(collapse="\n"),"\n\nNote that these formulas can only be applied to selected 'Output variables'")
        return(mess)
      }
    }
  }

  if (form != "")
    cat(paste0("Formulas    :\n\t", form %>% gsub(";","\n",.) %>% gsub("\n","\n\t",.), "\n"))
  cat("\n")

  name %<>% gsub(" ","",.)
  if (name != "") {
    if (exists("r_env")) {
      env <- r_env
    } else if (exists("r_data")) {
      env <- pryr::where("r_data")
    } else {
      return(object)
    }

    mess <- paste0("\n### Repeated simulation summary\n\nFunction:\n\n", fun, "\n\nDate: ",
                   lubridate::now())

    name <- paste0(name,"_",cfun)

    env$r_data[[name]] <- object
    env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique
    env$r_data[[paste0(name,"_descr")]] <- mess
  }

  sim_summary(object, fun = cfun)
}

#' Plot repeated simulation
#'
#' @param x Return value from \code{\link{repeater}}
#' @param sum_vars (Numerical) variables to summaries
#' @param byvar Variable(s) to group data by before summarizing
#' @param fun Functions to use for summarizing
#' @param form A string with the formula to evaluate (e.g., "profit = demand * (price - cost)")
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.repeater <- function(x,
                          sum_vars = "",
                          byvar = "sim",
                          fun = "sum_rm",
                          form = "",
                          shiny = FALSE, ...) {

  object <- x; rm(x)

  if (identical(sum_vars, "")) return(invisible())
  if (is.character(object)) object <- getdata(object)

  object %<>% group_by_(byvar) %>%
    summarise_each_(make_funs(fun), vars = sum_vars) %>%
    select(-1)

  if (length(sum_vars) == 1 && length(fun) > 1) colnames(object) <- paste0(sum_vars, "_", colnames(object))

  cleaner <- function(x) x %>% gsub("[ ]{2,}"," ",.) %>%
    gsub("[ ]*[\n;]+[ ]*",";",.) %>%
    gsub("[;]{2,}",";",.) %>%
    gsub(";$","",.)

  spliter <- function(x, symbol = " ") x %>% strsplit(., ";") %>% extract2(1) %>% strsplit(.,symbol)

  form %<>% cleaner
  if (form != "") {
    s <- form %>% gsub(" ","",.) %>% spliter("=")
    for (i in 1:length(s)) {
      obj <- s[[i]][1]
      fobj <- s[[i]][-1]
      if (length(fobj) > 1) fobj <- paste0(fobj, collapse = "=")
      out <- try(do.call(with, list(object, parse(text = fobj))), silent = TRUE)
      if (!is(out, 'try-error')) {
        object[[obj]] <- out
      } else {
        return(invisible)
      }
    }
  }

  plot_list <- list()
  for (i in colnames(object)) {
    dat <- select_(object, .dots = i)
    if (!does_vary(object[[i]])) next

    plot_list[[i]] <-
      visualize(select_(object, .dots = i), xvar = i, bins = 20, custom = TRUE)

    if (i %in% sum_vars) {
      cfun <- sub("_rm$","",fun)
      plot_list[[i]] <- plot_list[[i]] + xlab(paste0(cfun, " of ", i))
    }
  }

  sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = min(length(plot_list),2)))) ) %>%
    { if (shiny) . else print(.) }
}

sim_summary <- function(dat, dc = getclass(dat), fun = "") {

  isLogic <- "logical" == dc
  isNum <- !isLogic

  if (sum(isNum) > 0) {

    isConst <- !sapply(dat, does_vary) & isNum
    if (sum(isConst) > 0) {
      cn <- names(dc)[isConst]
      cat("Constants:\n")
      select(dat, which(isConst)) %>% na.omit %>% .[1,] %>% as.data.frame %>%
        set_rownames("") %>% set_colnames(cn) %>% print
      cat("\n")
    }

    isRnd <- isNum & !isConst
    if (sum(isRnd) > 0) {
      cn <- names(dc)[isRnd]
      cat("Random variables:\n")
      select(dat, which(isNum & !isConst)) %>%
        tidyr::gather_("variable", "values", cn) %>%
        group_by_("variable") %>%
        summarise_each(funs(n = length, mean = mean_rm, sd = sd_rm, min = min_rm, `5%` = p05, `25%` = p25,
                       median = median_rm, `75%` = p75, `95%` = p95, max = max_rm)) %>%
        { if (fun == "") . else {.[[1]] <- paste0(fun, " of ", .[[1]])}; . } %>%
        { .[[1]] <- format(.[[1]], justify = "left"); .} %>%
        data.frame(check.names = FALSE) %>%
        { .[,-1] %<>% round(.,3); colnames(.)[1] <- ""; . } %>%
        print(row.names = FALSE)
      cat("\n")
    }
  }

  if (sum(isLogic) > 0) {
    cat("Logicals:\n")
    select(dat, which(isLogic)) %>% summarise_each(funs(sum, mean)) %>% matrix(ncol = 2) %>%
      set_colnames(c("TRUE (nr)  ", "TRUE (prop)")) %>% set_rownames(names(dat)[isLogic]) %>% print
    cat("\n")
  }
}
