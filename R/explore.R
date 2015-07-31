#' Explore data
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param vars (Numerical) variables to summaries
#' @param byvar Variable(s) to group data by before summarizing
#' @param fun Functions to use for summarizing
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
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
#' @seealso \code{\link{plot.explore}} to plot summaries
#'
#' @export
explore <- function(dataset,
                    vars = "",
                    byvar = "",
                    fun = c("length", "mean_rm"),
                    data_filter = "") {

#   dataset <- "diamonds"
#   vars <- "price:x"
#   data_filter <- ""

  tvars <- vars
  if (!is_empty(byvar)) tvars %<>% c(byvar)

  dat <- getdata(dataset, tvars, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  ## in case : was used
  vars <- colnames(head(dat) %>% select_(.dots = vars))

  ## summaries only for numeric variables
  isNum <- getclass(dat) %>% {which("numeric" == . | "integer" == .)}

  if (is_empty(byvar)) {
    tab <- dat %>% select(isNum) %>% gather("Variable", "Value") %>% group_by_("Variable")  %>% summarise_each(make_funs(fun))
  } else {
    for(bv in byvar) if (!is.factor(dat[[bv]])) dat[[bv]] %<>% as.factor
    # isNum <- getclass(dat) %>% {which("numeric" == . | "integer" == .)}

    ## for median issue in dplyr < .5
    ## https://github.com/hadley/dplyr/issues/893
    tab <- dat %>% group_by_(.dots = byvar) %>%
      # select(isNum) %>%
      select(isNum) %>% mutate_each("as.numeric") %>%
      summarise_each(make_funs(fun))
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
#' @seealso \code{\link{plot.explore}} to plot summaries
#'
#' @export
summary.explore <- function(object, ...) {

  cat("Data     :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter   :", gsub("\\n","", object$data_filter), "\n")

  cat("\n")
#   if (class(object$tab) == "character") {
#     cat(paste0(object$tab[-length(object$tab)],sep="\n"))
#   } else {
    print(object$tab)

#     if (!exists("r_functions"))
#       funcs <- object$fun %>% set_names(.,.)
#     else
#       funcs <- get("r_functions")
#
#     for (f in object$fun) {
#       cat("Results grouped by: ", object$byvar, "\n")
#       cat("Function used: ", names(which(funcs == f)), "\n")
#       object$tab[[f]] %>%
#         { .[,-c(1:length(object$byvar))] %<>% round(3); . } %>%
#         print
#       cat("\n")
#     }
  # }

  invisible()
}

#' Make a tabel of summary statistics in DT
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant
#'
#' @param expl Return value from \code{\link{explorer}}
#' @param format Show Color bar ("color_bar"),  Heat map ("heat"), or None ("none")
#'
#' @examples
#' pivotr("diamonds", cvars = "cut") %>% make_dt
#' pivotr("diamonds", cvars = c("cut","clarity")) %>% make_dt(format = "color_bar")
#' ret <-  pivotr("diamonds", cvars = c("cut","clarity"), normalize = "total") %>%
#'    make_dt(format = "color_bar", check = "perc")
#'
#' @seealso \code{\link{pivotr}} to create the pivot-table using dplyr
#' @seealso \code{\link{summary.pivotr}} to print a plain text table
#'
#' @export
make_expl <- function(expl, format = "none") {

#   dt_tab <- expl$tab %>%
#     DT::datatable(rownames = FALSE,
#                   filter = list(position = "top", clear = FALSE, plain = TRUE),
#                   style = ifelse(expl$shiny, "bootstrap", "default"),
#                   options = list(
#                     search = list(regex = TRUE),
#                     processing = FALSE,
#                     pageLength = 10,
#                     lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","All"))
#                   )
#     )
#
#   return(dt_tab)

  tab <- expl$tab
  # cvar <- pvt$cvars[1]
  cvars <- expl$byvar # cvars %>% {if(length(.) > 1) .[-1] else .}
  cn <- colnames(tab) %>% {if(cvars == "") . else .[-which(cvars %in% .)]}

  ## column names without total
  # cn_nt <- if ("Total" %in% cn) cn[-which(cn == "Total")] else cn

#   tot <- tail(tab,1)[-(1:length(cvars))]
#   if ("perc" %in% check)
#     tot <- sprintf("%.2f%%", tot*100)
#   else
#     tot <- round(tot, 3)

  if (length(cvars) == 1 && cvar == cvars) {
    sketch = shiny::withTags(table(
      thead(
        tr(lapply(c(cvars,cn), th))
      ),
      tfoot(
        tr(lapply(c("Total",tot), th))
      )
    ))
  } else {
    sketch = shiny::withTags(table(
      thead(
        tr(
          th(colspan = length(c(cvars,cn)), cvar, class = "text-center")
        ),
        tr(
          lapply(c(cvars,cn), th)
        )
      ),
      tfoot(
        tr(
          th(colspan = length(cvars), "Total"), lapply(tot, th)
        )
      )
    ))
  }

  ## remove column totals
  ## should perhaps be part of pivotr but convenient for for now in tfoot
  ## and for external calls to pivotr
  tab <- filter(tab, tab[,1] != "Total")

  dt_tab <- tab %>%
    DT::datatable(container = sketch, rownames = FALSE,
                  filter = list(position = "top", clear = FALSE, plain = TRUE),
                  style = ifelse(pvt$shiny, "bootstrap", "default"),
                  # style = "bootstrap",
                  options = list(
                    # stateSave = TRUE,
                    search = list(regex = TRUE),
                    processing = FALSE,
                    pageLength = 10,
                    lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","All"))
                  )
    ) %>% DT::formatStyle(., cvars,  color = "white", backgroundColor = "grey") %>%
    {if ("Total" %in% cn) DT::formatStyle(., "Total", fontWeight = "bold") else .}

  ## heat map with red or color_bar
  if (format == "color_bar") {
    dt_tab %<>% DT::formatStyle(cn_nt,
                                background = DT::styleColorBar(range(tab[ , cn_nt], na.rm = TRUE), "lightblue"),
                                backgroundSize = "98% 88%",
                                backgroundRepeat = "no-repeat",
                                backgroundPosition = "center")
  } else if (format == "heat") {
    brks <- quantile(tab[, cn_nt], probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- seq(255, 40, length.out = length(brks) + 1) %>%
      round(0) %>%
      {paste0("rgb(255,", ., ",", .,")")}
    dt_tab %<>% DT::formatStyle(cn_nt, backgroundColor = DT::styleInterval(brks, clrs))
  }

  ## show percentage
  if ("perc" %in% check) dt_tab %<>% DT::formatPercentage(cn, 2)

  dt_tab

  ## can use this in R > Report inside Radiant but doesn't export
  # renderDataTable({make_dt(result)})
}




#' Plot method for the explore function
#'
#' @details See \url{http://vnijs.github.io/radiant/base/explore.html} for an example in Radiant. A plot will only be generated when a 'by' variable has been specified
#'
#' @param x Return value from \code{\link{explore}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew"))
#' plot(result)
#'
#' @seealso \code{\link{explore}} to generate summaries
#' @seealso \code{\link{summary.explore}} to show summaries
#'
#' @export
plot.explore <- function(x, shiny = FALSE, ...) {

  object <- x; rm(x)

  if (class(object$tab)[1] == "character")
    return(invisible())

  by_var <- fill_var <- object$byvar[1]
  if (length(object$byvar) > 1) fill_var <- object$byvar[2]

  if (!exists("r_functions")) {
    funcs <- object$fun %>% set_names(.,.)
  } else {
    funcs <- get("r_functions")
  }

  plots <- list()
  for (f in object$fun) {
    for (var in object$vars) {
      plots[[paste0(var,"_",f)]] <-
        ggplot(data = object$tab[[f]], aes_string(x = by_var, y = var, fill = fill_var)) +
          geom_bar(stat="identity", position = "dodge", alpha=.7) +
          ggtitle(paste("Function used:", names(which(funcs == f))))

      if (length(object$byvar) == 1) {
        plots[[paste0(var,"_",f)]] <- plots[[paste0(var,"_",f)]] +
          theme(legend.position = "none")
      }
    }
  }

  sshhr( do.call(arrangeGrob, c(plots, list(ncol = 1))) ) %>%
    { if (shiny) . else print(.) }
}

#' Create data.frame summary
#'
#' @details Used by Explore and Transform
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

