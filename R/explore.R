#' Explore data
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param vars (Numerical) variables to summaries
#' @param byvar Variable(s) to group data by before summarizing
#' @param fun Functions to use for summarizing
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param shiny Logical (TRUE, FALSE) to indicate if the function call originate inside a shiny app
#'
#' @return A list of all variables defined in the function as an object of class explore
#'
#' @examples
#' result <- explore("diamonds", "price:x")
#' summary(result)
#' result <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew"))
#' summary(result)
#' diamonds %>% explore("price", byvar = "cut", fun = c("length", "skew"))
#'
#' @seealso \code{\link{summary.explore}} to show summaries
#'
#' @export
explore <- function(dataset,
                    vars = "",
                    byvar = "",
                    fun = "mean_rm",
                    data_filter = "",
                    shiny = FALSE) {

  tvars <- vars
  if (!is_empty(byvar)) tvars %<>% c(byvar)

  dat <- getdata(dataset, tvars, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  ## in case : was used
  vars <- colnames(head(dat) %>% select_(.dots = vars))

  ## summaries only for numeric variables
  isNum <- getclass(dat) %>% {which("numeric" == . | "integer" == .)}

  ## avoid using .._rm as function name
  pfun <- make_funs(fun)

  if (is_empty(byvar)) {
    tab <- dat %>% select(isNum) %>%
      gather("variable", "value") %>%
      group_by_("variable")  %>% summarise_each(pfun)
    if (ncol(tab) == 2) colnames(tab) <- c("variable", names(pfun))
  } else {
    for(bv in byvar) if (!is.factor(dat[[bv]])) dat[[bv]] %<>% as.factor

    ## for median issue in dplyr < .5
    ## https://github.com/hadley/dplyr/issues/893
    tab <-
      dat %>% group_by_(.dots = byvar) %>%
      select(isNum) %>% mutate_each("as.numeric") %>%
      summarise_each(pfun)

    if (length(vars) > 1 && length(fun) > 1) {
      ## useful answer and comments: http://stackoverflow.com/a/27880388/1974918
      tab %<>% gather("variable", "value", -(1:length(byvar))) %>%
        separate(variable, into = c("variable", "fun"), sep = "_(?=[^_]*$)") %>%
        mutate(fun = factor(fun, levels = names(pfun)), variable = factor(variable, levels = vars)) %>%
        spread_("fun","value")
    } else if (length(fun) == 1) {
      tab %<>% gather("variable", "value", -(1:length(byvar))) %>%
        mutate(variable = factor(variable, levels = vars)) %>%
        rename_(.dots = setNames("value", names(pfun)))
    } else if (length(vars) == 1){
      tab %<>% mutate(variable = factor(vars, levels = vars)) %>%
        select_(.dots = c(byvar, "variable", names(pfun)))
    }
  }

  ## dat no longer needed
  rm(dat)

  environment() %>% as.list %>% set_class(c("explore", class(.)))
}

#' Summary method for the explore function
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{explore}}
#' @param top The variable (type) to display at the top of the table
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- explore("diamonds", "price:x")
#' summary(result)
#' result <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew"))
#' summary(result)
#' diamonds %>% explore("price:x") %>% summary
#' diamonds %>% explore("price", byvar = "cut", fun = c("length", "skew")) %>% summary
#'
#' @seealso \code{\link{explore}} to generate summaries
#'
#' @export
summary.explore <- function(object, top = "fun", ...) {

  cat("Data      :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter    :", gsub("\\n","", object$data_filter), "\n")
  if (object$byvar != "")
    cat("Grouped by: ", object$byvar, "\n")
  cat("Functions : ", names(object$pfun), "\n")
  cat("Top       : ", c("fun" = "Function", "var" = "Variables", "byvar" = "Group by")[top], "\n")
  cat("\n")

  tab <- object %>% flip(top) %>% as.data.frame
  cn_all <- colnames(tab)
  cn_num <- cn_all[sapply(tab, is.numeric)]
  tab[,cn_num] %<>% round(3)

  print(tab, row.names = FALSE)

  invisible()
}

#' Flip the DT table to put Function, Variable, or Group by on top
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant
#'
#' @param expl Return value from \code{\link{explore}}
#' @param top The variable (type) to display at the top of the table ("fun" for Function, "var" for Variable, and "byvar" for Group by
#'
#' @examples
#' result <- explore("diamonds", "price:x") %>% flip("var")
#' result <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew")) %>%
#'   flip("byvar")
#'
#' @seealso \code{\link{explore}} to generate summaries
#' @seealso \code{\link{make_expl}} to create the DT table
#'
#' @export
flip <- function(expl, top) {
  flip
  cvars <- expl$byvar %>% {if (.[1] == "") character(0) else .}
  if (top[1] == "var")
    expl$tab %>% gather("function", "value", -(1:(length(cvars)+1))) %>% spread_("variable", "value")
  else if (top[1] == "byvar" && length(cvars) > 0)
    expl$tab %>% gather("function", "value", -(1:(length(cvars)+1))) %>% spread_(cvars[1], "value")
  else
    expl$tab
}

#' Make a tabel of summary statistics in DT
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant
#'
#' @param expl Return value from \code{\link{explore}}
#' @param top The variable (type) to display at the top of the table ("fun" for Function, "var" for Variable, and "byvar" for Group by
#' @param dec Number of decimals to show
#'
#' @examples
#' tab <- explore("diamonds", "price:x") %>% make_expl
#' tab <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew")) %>%
#'   make_expl(top = "byvar")
#'
#' @seealso \code{\link{pivotr}} to create the pivot-table using dplyr
#' @seealso \code{\link{summary.pivotr}} to print a plain text table
#'
#' @export
make_expl <- function(expl, top = "fun", dec = 3) {

  tab <- expl %>% flip(top)
  cn_all <- colnames(tab)
  cn_num <- cn_all[sapply(tab, is.numeric)]
  cn_cat <- cn_all[-which(cn_all %in% cn_num)]

  top <- c("fun" = "Function", "var" = "Variables", "byvar" = paste0("Group by: ", expl$byvar[1]))[top]

  sketch = shiny::withTags(table(
    thead(
      tr(
        th(" ", colspan = length(cn_cat)),
        lapply(top, th, colspan = length(cn_num), class = "text-center")
      ),
      tr(lapply(cn_all, th))
    )
  ))

  dt_tab <- tab %>% {.[,cn_num] <- round(.[,cn_num], dec); .} %>%
    DT::datatable(container = sketch, rownames = FALSE,
                  filter = list(position = "top", clear = FALSE, plain = TRUE),
                  style = ifelse(expl$shiny, "bootstrap", "default"),
                  options = list(
                    # stateSave = TRUE,
                    search = list(regex = TRUE),
                    processing = FALSE,
                    pageLength = 10,
                    lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","All"))
                  )
    ) %>% DT::formatStyle(., cn_cat,  color = "white", backgroundColor = "grey")

  ## heat map with red or color_bar
  # if (format == "color_bar") {
  #   for (i in cn_num) {
  #     dt_tab %<>% DT::formatStyle(i,
  #                                 background = DT::styleColorBar(range(tab[, i], na.rm = TRUE), "lightblue"),
  #                                 backgroundSize = "98% 88%",
  #                                 backgroundRepeat = "no-repeat",
  #                                 backgroundPosition = "center")
  #   }
  # } else if (format == "heat") {
  #   for (i in cn_num) {
  #     brks <- quantile(tab[[i]], probs = seq(.05, .95, .05), na.rm = TRUE)
  #     clrs <- seq(255, 40, length.out = length(brks) + 1) %>%
  #       round(0) %>%
  #       {paste0("rgb(255,", ., ",", .,")")}
  #     dt_tab %<>% DT::formatStyle(i, backgroundColor = DT::styleInterval(brks, clrs))
  #   }
  # }

  dt_tab

  ## can use this in R > Report inside Radiant but doesn't export
  # renderDataTable({make_dt(result)})
}

#' Create data.frame summary
#'
#' @details Used by Transform
#'
#' @param dat Data.frame
#' @param dc Class for each variable
#'
#' @export
getsummary <- function(dat, dc = getclass(dat)) {

  isFct <- "factor" == dc
  isNum <- "numeric" == dc | "integer" == dc
  isDate <- "date" == dc
  isChar <- "character" == dc
  isLogic <- "logical" == dc
  isPeriod <- "period" == dc

  if (sum(isNum) > 0) {

    cn <- names(dc)[isNum]

    cat("Summarize numeric variables:\n")
    select(dat, which(isNum)) %>%
      tidyr::gather_("variable", "values", cn) %>%
      group_by_("variable") %>%
      summarise_each(funs(n = length, missing = nmissing, mean = mean_rm,
                     median = median_rm, min = min_rm, max = max_rm,
                     `25%` = p25, `75%` = p75, sd = sd_rm, se = serr,
                     cv = sd/mean)) %>%
      data.frame(check.names = FALSE) %>%
      { .[,-1] %<>% round(.,3); colnames(.)[1] <- ""; . } %>%
      print(row.names = FALSE)
    cat("\n")
  }
  if (sum(isFct) > 0) {
    cat("Summarize factors:\n")
    select(dat, which(isFct)) %>% summary %>% print
    cat("\n")
  }
  if (sum(isDate) > 0) {
    cat("Earliest dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(min)) %>% print
    cat("\nFinal dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(max)) %>% print
    cat("\n")
  }
  if (sum(isPeriod) > 0) {
    cat("Earliest time:\n")
    select(dat, which(isPeriod)) %>% summarise_each(funs(min)) %>% print
    cat("\nFinal time:\n")
    select(dat, which(isPeriod)) %>% summarise_each(funs(max)) %>% print
    cat("\n")
  }
  if (sum(isChar) > 0) {
    cat("Summarize character variables (< 20 unique values shown):\n")
    select(dat, which(isChar)) %>% distinct %>% lapply(unique) %>%
      {for(i in names(.)) cat(i, ":", .[[i]][1:min(20,length(.[[i]]))], "\n")}
    cat("\n")
  }
  if (sum(isLogic) > 0) {
    cat("Summarize logical variables:\n")
    select(dat, which(isLogic)) %>% tally %>% print
    cat("\n")
  }
}

###########################################
## turn functions below into functional ...
###########################################

#' Number of missing values
#' @param x Input variable
#' @return number of missing values
#' @examples
#' nmissing(c("a","b",NA))
#'
#' @export
nmissing <- function(x) sum(is.na(x))

#' 25th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 25th percentile
#' @examples
#' p25(rnorm(100))
#'
#' @export
p25 <- function(x, na.rm = TRUE) quantile(x,.25, na.rm = na.rm)

#' 75th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 75th percentile
#' @examples
#' p75(rnorm(100))
#'
#' @export
p75 <- function(x, na.rm = TRUE) quantile(x,.75, na.rm = na.rm)

#' Standard error
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Standard error
#' @examples
#' serr(rnorm(100))
#'
#' @export
serr <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / length(na.omit(x))

#' Coefficient of variation
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Coefficient of variation
#' @examples
#' cv(runif (100))
#'
#' @export
cv <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)

#' Mean with na.rm = TRUE
#' @param x Input variable
#' @return Mean value
#' @examples
#' mean_rm(runif (100))
#'
#' @export
mean_rm <- function(x) mean(x, na.rm = TRUE)

#' Median with na.rm = TRUE
#' @param x Input variable
#' @return Median value
#' @examples
#' median_rm(runif (100))
#'
#' @export
median_rm <- function(x) median(x, na.rm = TRUE)

#' Min with na.rm = TRUE
#' @param x Input variable
#' @return Minimum value
#' @examples
#' min_rm(runif (100))
#'
#' @export
min_rm <- function(x) min(x, na.rm = TRUE)

#' Max with na.rm = TRUE
#' @param x Input variable
#' @return Maximum value
#' @examples
#' max_rm(runif (100))
#'
#' @export
max_rm <- function(x) max(x, na.rm = TRUE)

#' Standard deviation with na.rm = TRUE
#' @param x Input variable
#' @return Standard deviation
#' @examples
#' sd_rm(rnorm(100))
#'
#' @export
sd_rm <- function(x) sd(x, na.rm = TRUE)

#' Sum with na.rm = TRUE
#' @param x Input variable
#' @return Sum of input values
#' @examples
#' sum_rm(1:200)
#'
#' @export
sum_rm <- function(x) sum(x, na.rm = TRUE)

#' Make a list of functions-as-formulas to pass to dplyr
#' @param x List of functions as strings
#' @return List of functions to pass to dplyr in formula form
#' @examples
#' make_funs(c("mean", "sum_rm"))
#'
#' @export
make_funs <- function(x) {
  xclean <- gsub("_rm$","",x) %>% sub("length","n",.) %>%  sub("nmissing","missing",.)
  env <- if (exists("radiant")) environment(radiant::radiant) else parent.frame()
  dplyr::funs_(lapply(paste0(xclean, " = ~", x), as.formula, env = env) %>% setNames(xclean))
}
