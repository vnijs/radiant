### Make another tab where you can run multiple sims (e.g., grid-search) and
### aggregate the results
### This would also be useful for Conjoint I think

#' Simulate data for decision analysis
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/simulater.html} for an example in Radiant
#'
#' @param const A string listing the constants to include in the analysis (e.g., "cost = 3; size = 4")
#' @param lnorm A string listing the log-normally distributed random variables to include in the analysis (e.g., "demand 2000 1000" where the first number is the log-mean and the second is the log-standard deviation)
#' @param norm A string listing the normally distributed random variables to include in the analysis (e.g., "demand 2000 1000" where the first number is the mean and the second is the standard deviation)
#' @param unif A string listing the uniformly distributed random variables to include in the analysis (e.g., "demand 0 1" where the first number is the minimum value and the second is the maximum value)
#' @param discrete A string listing the random variables with a discrete distribution to include in the analysis (e.g., "price 5 .3 8 .7" where for each pair of numbers the first is the value and the second the probability
#' @param binom A string listing the random variables with a binomail distribution to include in the analysis (e.g., "crash 100 .01") where the first number is the number of trials and the second is the probability of success)
#' @param sequ A string listing the start and end for a sequence to include in the analysis (e.g., "trend 1 100 1"). The number of 'steps' is determined by the number of simulations.
#' @param grid A string listing the start, end, and step for a set of sequences to include in the analysis (e.g., "trend 1 100 1"). The number of rows in the expanded will over ride the number of simulations
#' @param data Name of a dataset to be used in the calculations
#' @param form A string with the formula to evaluate (e.g., "profit = demand * (price - cost)")
#' @param seed To repeat a simulation with the same randomly generated values enter a number into Random seed input box.
#' @param name To save the simulated data for further analysis specify a name in the Sim name input box. You can then investigate the simulated data by choosing the specified name from the Datasets dropdown in any of the other Data tabs.
#' @param nr Number of simulations
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
                      lnorm = "",
                      norm = "",
                      unif = "",
                      discrete = "",
                      binom = "",
                      sequ = "",
                      grid = "",
                      data = "",
                      form = "",
                      seed = "",
                      name = "",
                      nr = 1000,
                      dat = NULL) {

  # print(environment())
  # print(parent.frame())
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

  # dat <- list(a = 1:10, b = 8)
  # do.call(with, list(dat, parse(text = "a * b")))

  # library(magrittr)
  ## parsing grid
  # grid <- "S 0 6 1; B 0 9 1"
  grid %<>% sim_cleaner
  if (grid != "") {
    s <- grid %>% sim_splitter
    for (i in 1:length(s)) {
      if (is_empty(s[[i]][4])) s[[i]][4] <- 1
      s[[i]] %>% { dat[[.[1]]] <<- seq(as.numeric(.[2]) , as.numeric(.[3]), as.numeric(.[4]))}
    }
    dat <- as.list(expand.grid(dat) %>% as_data_frame)
    nr <- length(dat[[1]])
  }

  if (is_empty(nr)) {
    mess <- c("error",paste0("Please specify the number of simulations in '# sims'"))
    return(mess %>% set_class(c("simulater", class(.))))
  }

  ## parsing constant
  # const <- "non_labor_cost 3995;\n;\n;cost 11\n   \n \n\n\n\n\n     \n    \n   "
  const %<>% sim_cleaner
  if (const != "") {
    s <- const %>% sim_splitter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- as.numeric(.[2]) %>% rep(nr) }
  }

  ## parsing uniform
  unif %<>% sim_cleaner
  if (unif != "") {
    s <- unif %>% sim_splitter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- runif(nr, as.numeric(.[2]) , as.numeric(.[3]))}
  }

  ## parsing log normal
  lnorm %<>% sim_cleaner
  if (lnorm != "") {
    s <- lnorm %>% sim_splitter
    for (i in 1:length(s)) {
      sdev <- as.numeric(s[[i]][3])
      if (!sdev > 0) {
        mess <- c("error",paste0("All log-normal variables should have a standard deviation larger than 0.\nPlease review the input carefully"))
        return(mess %>% set_class(c("simulater", class(.))))
      }
      # m <- as.numeric(s[[i]][2])
      # if (!m > 1) {
      #   mess <- c("error",paste0("All log-normal variables should have a mean larger than 1.\nPlease review the input carefully"))
      #   return(mess %>% set_class(c("simulater", class(.))))
      # }
      s[[i]] %>% { dat[[.[1]]] <<- rlnorm(nr, as.numeric(.[2]), sdev)}
      # s[[i]] %>% { dat[[.[1]]] <<- rlnorm(nr, m, sdev)}
    }
  }

  ## parsing normal
  norm %<>% sim_cleaner
  if (norm != "") {
    s <- norm %>% sim_splitter
    for (i in 1:length(s)) {
      sdev <- as.numeric(s[[i]][3])
      if (!sdev > 0) {
        mess <- c("error",paste0("All normal variables should have a standard deviation larger than 0.\nPlease review the input carefully"))
        return(mess %>% set_class(c("simulater", class(.))))
      }
      s[[i]] %>% { dat[[.[1]]] <<- rnorm(nr, as.numeric(.[2]) , sdev)}
    }
  }

  ## parsing binomial
  binom %<>% sim_cleaner
  if (binom != "") {
    s <- binom %>% sim_splitter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- rbinom(nr, as_integer(.[2]) , as_numeric(.[3]))}
  }

  ## parsing sequence
  sequ %<>% sim_cleaner
  if (sequ != "") {
    s <- sequ %>% sim_splitter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- seq(as.numeric(.[2]) , as.numeric(.[3]), length.out = as.numeric(nr))}
  }

  ## adding data to dat list
  if (data != "" && data != "none") {
    sdat <- getdata(data)
    for (i in colnames(sdat))
      dat[[i]] <- sdat[[i]]
  }

  ## parsing discrete
  # discrete = "price 6 .30 8 .70"
  discrete %<>% sim_cleaner
  if (discrete != "") {
    s <- discrete %>% sim_splitter
    for (i in 1:length(s)) {

      dpar <- sshhr( try(s[[i]][-1] %>% as.numeric %>% matrix(ncol = 2), silent = TRUE) )
      if (is(dpar, 'try-error') || any(is.na(dpar))) {
        mess <- c("error",paste0("Input for a discrete variable contains an error. Please review the input carefully"))
        return(mess %>% set_class(c("simulater", class(.))))
      } else if (sum(dpar[,2]) != 1) {
        mess <- c("error",paste0("Probabilities for a discrete variable do not sum to 1 (",round(sum(dpar[,2]),3),")"))
        return(mess %>% set_class(c("simulater", class(.))))
      }

      dat[[s[[i]][1]]] <- sample(dpar[,1], nr, replace = TRUE, prob = dpar[,2])
    }
  }

  # f <- seq(0,1,0.01)
  # fobj <- "f %*% GOOG + (1-f) %*% MSFT"
  # do.call(with, list(stocks, parse(text = fobj)))

  # f %*% GOOG
  # class(f)
  # f
  # class(GOOG)
  # f %*% GOOG
  # library(dplyr)
  # expand.grid(f = f,GOOG = GOOG) %>% group_by(f) %>% summarize(sd(GOOG))
  # expand.grid(f = f, attach(stocks))


  #  <- function(x, fobj, stocks) do.call(with, list(stocks, parse(text = fobj)))
  # ?by

  # sapply(f, )


  ## parsing formula
  # form <- "profit = (price - cost)*nr_meals - labor_cost - non_labor_cost\n ; \n margin = price - cost ;;;   ; \n  \n  "
  # form = "demand = demand -50*price;profit = demand*(price-var_cost) - fixed_cost"
  # form = "demand = demand - .1*lag(demand, 0);profit = demand*(price-var_cost) - fixed_cost"
  # form = "demand = demand - .1*lag(demand, default=0);# profit = demand*(price-var_cost) - fixed_cost"
  # form = "#demand = demand - .1*lag(demand, default=0);\n# profit = demand*(price-var_cost) - fixed_cost"
  # form %>% gsub("\\s{2,}"," ",.) %>%
  # gsub("^\\s*#\\s*.*\n","",.) %>%
  # gsub("\\s*[\n;]+\\s*",";",.) %>%
  # gsub("[;]{2,}",";",.) %>%
  # gsub(";$","",.) %>%
  # gsub("^;","",.)

  # sim_splitter <- function(x, symbol = " ") x %>% strsplit(., ";") %>% extract2(1) %>% strsplit(.,symbol)

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

  form %<>% sim_cleaner
  if (form != "") {
    s <- form %>% gsub("\\s+","",.) %>% sim_splitter("=")
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

  ## removing data to dat list
  if (data != "" && data != "none") {
    for (i in colnames(sdat)) dat[[i]] <- NULL
  }

  # print("----")
  # print(names(dat))

  ret <- list(dat = as.data.frame(dat) %>% na.omit, sim_call = as.list(match.call())[-1]) %>%
    set_class(c("simulater", class(.)))

  if (nrow(ret$dat) == 0) {
    mess <- c("error",paste0("The simulated data set has 0 rows"))
    return(mess %>% set_class(c("simulater", class(.))))
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
#' @param dec Number of decimals to show
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
summary.simulater <- function(object, dec = 4, ...) {

  if (is.character(object)) {
    if (object[1] == "error") return(cat(object[2]))
    else object <- getdata(object)
  }

  cat("Simulation\n")
  # cat("Simulations:", object$sim_call$nr, "\n")
  cat("Simulations:", nrow(object$dat), "\n")
  cat("Random seed:", object$sim_call$seed, "\n")
  cat("Sim data   :", object$sim_call$name, "\n")
  if (!is_empty(object$sim_call$binom))
    cat("Binomial   :", gsub(";", "; ", object$sim_call$binom) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$const))
    cat("Constant   :", gsub(";", "; ", object$sim_call$const) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$discrete))
    cat("Discrete   :", gsub(";", "; ", object$sim_call$discrete) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$lnorm))
    cat("Log normal :", gsub(";", "; ", object$sim_call$lnorm) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$norm))
    cat("Normal     :", gsub(";", "; ", object$sim_call$norm) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$unif))
    cat("Uniform    :", gsub(";", "; ", object$sim_call$unif) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$sequ))
    cat("Sequence   :", gsub(";", "; ", object$sim_call$sequ) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$grid))
    cat("Grid search:", gsub(";", "; ", object$sim_call$grid) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$data))
    cat("Data       :", gsub(";", "; ", object$sim_call$data) %>% gsub("\\n","",.), "\n")
  if (!is_empty(object$sim_call$form))
    # cat(paste0("Formulas   :\n\t", object$sim_call$form %>% gsub(";","\n",.) %>% gsub("\n","\n\t",.), "\n"))
    cat(paste0("Formulas   :\n\t", object$sim_call$form %>% gsub("\n","\n\t",.), "\n"))
  cat("\n")

  sim_summary(object$dat, dec = ifelse(is.na(dec), 4, dec))
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

  sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = min(length(plot_list),2)))) ) %>%
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

  if (is_empty(nr)) {
    if (is_empty(grid)) {
      mess <- c("error",paste0("Please specify the number of repetitions in '# reps'"))
      return(mess %>% set_class(c("repeater", class(.))))
    } else {
      nr = 1
    }
  }

  if (is.character(sim)) sim <- getdata(sim)
  seed %>% gsub("[^0-9]","",.) %>% { if (. != "") set.seed(seed) }

  if (identical(vars, "") && identical(grid, "")) {
    mess <- c("error",paste0("Select variables to re-simulate and/or a specify a constant\nto change using 'Grid search'"))
    return(mess %>% set_class(c("repeater", class(.))))
  }

  # grid <- "q 100 200 1; a 1 5 1"
  # grid <- "q 100 200 1"
  # library(magrittr)
  # vars <- c("a","b")
  if (identical(vars, "")) vars <- character(0)

  grid_list <- list()
  if (!identical(grid, "")) {
    grid %<>% sim_cleaner
    if (grid != "") {
      s <- grid %>% sim_splitter
      for (i in 1:length(s)) {
        if (is_empty(s[[i]][4])) s[[i]][4] <- 1
        s[[i]] %>% { grid_list[[.[1]]] <<- seq(as.numeric(.[2]) , as.numeric(.[3]), as.numeric(.[4]))}
      }
    }
    ## expanding list of variables but removing ""
    vars <- c(vars, names(grid_list)) %>% unique
  }

  ## from http://stackoverflow.com/a/7664655/1974918
  ## keep those list elements that, e.g., q is in
  nr_sim <- nrow(sim$dat)
  sc <- sim$sim_call

  if (!is_empty(sc$data)) vars <- c(sc$data, vars)

  sc$name <- sc$seed <- "" ## cleaning up the sim call
  sc_keep <- grep(paste(vars, collapse = "|"), sc, value=TRUE)
  sc[1:which(names(sc) == "form")] <- ""
  sc[names(sc_keep)] <- sc_keep
  sc$dat <- sim$dat %>% as.list


  ## testing
  ## testing
  ## testing
  ## testing
  # sc$data <- "stocks"
  ## testing
  ## testing
  ## testing
  ## testing

  # print(sc_keep)
  # print(sc)

  # mess <- c("error","test")
  # return(mess %>% set_class(c("repeater", class(.))))

  rep_sim <- function(rep_nr) {
    bind_cols(
      data_frame(rep = rep(rep_nr, nr_sim), sim = 1:nr_sim),
      do.call(simulater, sc)$dat
    ) %>% na.omit
  }

  rep_grid_sim <- function(gval) {
    gvars <- names(gval)

    ## removing form ...
    sc_grid <- grep(paste(gvars, collapse = "|"), sc_keep, value=TRUE) %>% {.[which(names(.) != "form")]}
    for (i in 1:length(gvars)) {
      sc_grid %<>% sub(paste0("[;\n]", gvars[i], " [.0-9]+"), paste0("\n", gvars[i], " ", gval[gvars[i]]), .) %>%
      sub(paste0("^", gvars[i], " [.0-9]+"), paste0(gvars[i], " ", gval[gvars[i]]), .)
    }

    # print(sc_grid)
    # print(sc)

    sc[names(sc_grid)] <- sc_grid

    # print(sc)

    # print(do.call(simulater, sc))
    # return()

    bind_cols(
      data_frame(rep = rep(paste(gval, collapse = "/"), nr_sim), sim = 1:nr_sim),
      do.call(simulater, sc)$dat
    ) %>% na.omit

    # mtcars

  }


  if (length(grid_list) == 0) {
    ret <- bind_rows(lapply(1:nr, rep_sim)) %>% set_class(c("repeater", class(.)))
  } else {
    grid <- expand.grid(grid_list)

   # print(apply(grid, 1, rep_grid_sim))

  # mess <- c("error","test")
  # return(mess %>% set_class(c("repeater", class(.))))

    ret <- bind_rows(apply(grid, 1, rep_grid_sim)) %>% set_class(c("repeater", class(.)))
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
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.repeater <- function(object,
                             sum_vars = "",
                             byvar = "",
                             fun = "sum_rm",
                             form = "",
                             name = "",
                             dec = 4,
                             ...) {

  if (identical(sum_vars, "")) return("Select one or more 'Output variables'")

  if (is.character(object)) {
    if (object[1] == "error") return(cat(object[2]))
    else object <- getdata(object)
  }

  sim <- max(object$sim)
  reps <- length(unique(object$rep))

  ## legacy for when 'rep' was called 'run'
  if (byvar == "run") byvar <- "rep"

  ## show results
  cat("Repeated simulation\n")
  cat("Simulations :", sim, "\n")
  cat("Repetitions :", reps, "\n")
  cat("Group by    :", ifelse (byvar == "rep", "Repeat", "Simulation"), "\n")
  cfun <- sub("_rm$","",fun)
  cat("Function    :", cfun, "\n")
  # cat("Random  seed:", object$sim_call$seed, "\n")
  cat("Repeat  data:", name, "\n")
  cat("Summary data:", paste0(name,"_",cfun) , "\n")

  if (fun != "none") {
    object %<>% group_by_(byvar) %>%
      summarise_each_(make_funs(fun), vars = sum_vars) %>%
      select(-1)

    if (length(sum_vars) == 1 && length(fun) > 1) colnames(object) <- paste0(sum_vars, "_", colnames(object))
  } else {
    object %<>% select_(.dots = sum_vars)
  }

  form %<>% sim_cleaner
  if (form != "") {
    s <- form %>% gsub("\\s+","",.) %>% sim_splitter("=")
    for (i in 1:length(s)) {
      if (grepl("^#",s[[i]][1])) next
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

    if (fun != "none") {
      mess <- paste0("\n### Repeated simulation summary\n\nFunction:\n\n", fun, "\n\nDate: ",
                     lubridate::now())
    } else {
      mess <- paste0("\n### Repeated simulation:\n\nDate: ", lubridate::now())
    }

    name <- paste0(name,"_",cfun)

    env$r_data[[name]] <- object
    env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique
    env$r_data[[paste0(name,"_descr")]] <- mess
  }

  sim_summary(object, fun = cfun, dec = ifelse (is.na(dec), 4, dec))
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

  if (identical(sum_vars, "")) return(invisible())

  if (is.character(x)) {
    if (x[1] == "error") return(invisible())
    object <- getdata(x)
    if (nrow(object) == 0) return(invisible())
  }
  rm(x)

  if (fun != "none") {
    object %<>% group_by_(byvar) %>%
      summarise_each_(make_funs(fun), vars = sum_vars) %>%
      select(-1)

    if (length(sum_vars) == 1 && length(fun) > 1) colnames(object) <- paste0(sum_vars, "_", colnames(object))
  } else {
    object %<>% select_(.dots = sum_vars)
  }

  form %<>% sim_cleaner
  if (form != "") {
    s <- form %>% gsub(" ","",.) %>% sim_splitter("=")
    for (i in 1:length(s)) {
      if (grepl("^#",s[[i]][1])) next
      obj <- s[[i]][1]
      fobj <- s[[i]][-1]
      if (length(fobj) > 1) fobj <- paste0(fobj, collapse = "=")
      out <- try(do.call(with, list(object, parse(text = fobj))), silent = TRUE)
      if (!is(out, 'try-error')) {
        object[[obj]] <- out
      } else {
        return(invisible())
      }
    }
  }

  plot_list <- list()
  for (i in colnames(object)) {
    dat <- select_(object, .dots = i)
    if (!does_vary(object[[i]])) next

    plot_list[[i]] <-
      visualize(select_(object, .dots = i), xvar = i, bins = 20, custom = TRUE)

    if (i %in% sum_vars && fun != "" && fun != "none") {
        cfun <- sub("_rm$","",fun)
        plot_list[[i]] <- plot_list[[i]] + xlab(paste0(cfun, " of ", i))
    }
  }

  if (length(plot_list) == 0) return(invisible())

  sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = min(length(plot_list),2)))) ) %>%
    { if (shiny) . else print(.) }
}

#' Print simulation summary
#'
#' @param dat Simulated data
#' @param dc Variable classes
#' @param fun Summary function to apply
#' @param dec Number of decimals to show
#'
#' @export
sim_summary <- function(dat, dc = getclass(dat), fun = "", dec = 4) {

  isLogic <- "logical" == dc
  isNum <- !isLogic

  dec <- ifelse(is.na(dec), 4, as_integer(dec))

  if (sum(isNum) > 0) {

    isConst <- !sapply(dat, does_vary) & isNum
    if (sum(isConst) > 0) {
      cn <- names(dc)[isConst]
      cat("Constants:\n")
      select(dat, which(isConst)) %>% na.omit %>% .[1,] %>% as.data.frame %>%
        round(dec) %>% mutate_each(funs(formatC(., big.mark = ",", digits = dec, format = "f"))) %>%
        set_rownames("") %>% set_colnames(cn) %>%
        print
      cat("\n")
    }

    isRnd <- isNum & !isConst
    if (sum(isRnd) > 0) {
      cn <- names(dc)[isRnd]
      cat("Variables:\n")
      select(dat, which(isNum & !isConst)) %>%
        tidyr::gather_("variable", "values", cn) %>%
        group_by_("variable") %>%
        summarise_each(funs(n = length, mean = mean_rm, sd = sd_rm, min = min_rm, `5%` = p05, `25%` = p25,
                       median = median_rm, `75%` = p75, `95%` = p95, max = max_rm)) %>%
        { if (fun == "" || fun == "none") . else {.[[1]] <- paste0(fun, " of ", .[[1]])}; . } %>%
        { .[[1]] <- format(.[[1]], justify = "left"); .} %>%
        data.frame(check.names = FALSE) %>%
        { .[,-1] %<>% round(.,dec); colnames(.)[1] <- ""; . } %>%
        { .[,-(1:2)] %<>% mutate_each(funs(formatC(., big.mark = ",", digits = dec, format = "f"))); . } %>%
        print(row.names = FALSE)
      cat("\n")
    }
  }

  if (sum(isLogic) > 0) {
    cat("Logicals:\n")
    select(dat, which(isLogic)) %>% summarise_each(funs(sum, mean)) %>% round(dec) %>%
      matrix(ncol = 2) %>% set_colnames(c("TRUE (nr)  ", "TRUE (prop)")) %>%
      set_rownames(names(dat)[isLogic]) %>% print
    cat("\n")
  }
}

#' Clean input command string
#'
#' @param x Input string
#'
#' @return Cleaned string
#'
#' @export
sim_cleaner <- function(x) x %>% gsub("[ ]{2,}"," ",.) %>%
  gsub("[ ]*[\n;]+[ ]*",";",.) %>%
  gsub("[;]{2,}",";",.) %>%
  gsub(";$","",.) %>%
  gsub("^;","",.)

#' Split input command string
#'
#' @param x Input string
#' @param symbol Symbol used to split the command string
#'
#' @return Split input command string
#'
#' @export
sim_splitter <- function(x, symbol = " ") x %>% strsplit(., ";") %>% extract2(1) %>% strsplit(.,symbol)

#' Find maxium value of a vector
#'
#' @param var Variable to find the maximum for
#' @param val Variable to find the value for at the maxium of var
#'
#' @return Value of val at the maximum of var
#'
#' @export
find_max <- function(var, val = "") {
  if (is_empty(val)) stop("Error in find_max (2 inputs required)\nSpecify the variable to evaluate at the maxium of the first input")
  val[which.max(var)]
}

#' Find minimum value of a vector
#'
#' @param var Variable to find the minimum for
#' @param val Variable to find the value for at the maxium of var
#'
#' @return Value of val at the minimum of var
#'
#' @export
find_min <- function(var, val = "") {
  if (is_empty(val)) stop("Error in find_min (2 inputs required)\nSpecify the variable to evaluate at the minimum of the first input")
  val[which.min(var)]
}

#' Standard deviation of weighted sum of variables
#'
#' @param ... A matched number of weights and stocks
#'
#' @return A vector of standard deviation estimates
#'
#' @export
sdw <- function(...) {
  dl <- list(...)
  nr <- length(dl)/2
  w <- data.frame(dl[1:nr])
  d <- data.frame(dl[(nr+1):length(dl)])
  apply(w, 1, function(w) sd(rowSums(sweep(d, 2, w, "*"))))
}
