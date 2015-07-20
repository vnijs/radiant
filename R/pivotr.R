#' Create a pivot table using dplyr
#'
#' @details Create a pivot-table. See \url{http://vnijs.github.io/radiant/base/pivotr.html} for an example in Radiant
#'
#' @param dataset Name of the dataframe to change
#' @param cvars Categorical variables
#' @param nvar Numerical variable
#' @param fun Function to apply to numerical variable
#' @param normalize Normalize the table by "row" total,"colum" totals, or overall "total"
#' @param data_filter Expression used to filter the dataset. This should be a string (e.g., "price > 10000")
#' @param shiny Logical (TRUE, FALSE) to indicate if the function call originate inside a shiny app
#'
#' @examples
#' pivotr("diamonds", cvars = "cut")$tab
#' pivotr("diamonds", cvars = c("cut","clarity","color"))$tab
#' pivotr("diamonds", cvars = "cut:clarity", nvar = "price")$tab
#'
#' @export
pivotr <- function(dataset,
                   cvars = "",
                   nvar = "None",
                   fun = "mean",
                   normalize = "None",
                   data_filter = "",
                   shiny = FALSE) {

  vars <- if (nvar == "None") cvars else c(cvars, nvar)
  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  ## in case : was used vor cvars
  if (length(vars) < ncol(dat))
    cvars <- colnames(dat) %>% {.[. != nvar]}

  if (nvar == "None") nvar <- "n"

  for (cv in cvars)
    if (!is.factor(dat[[cv]])) dat[[cv]] %<>% as.factor
  ind <- ifelse(length(cvars) > 1, -1, 1)
  levs <- lapply(select_(dat, .dots = cvars[ind]), levels)

  sel <- function(x, nvar) if (nvar == "n") x else select_(x, .dots = nvar)
  sfun <- function(x, nvar, cvars = "", fun = fun) {
    if (nvar == "n")
      if (all(cvars == "")) count_(x) else count_(x, cvars)
    else
      mutate_each_(x, "as.numeric", vars = nvar) %>%
      summarise_each_(as.formula(paste0("~",fun)), vars = nvar)
  }

  ## main tab
  tab <-
    dat %>%
    group_by_(.dots = cvars) %>%
    sfun(nvar, cvars, fun)

  ## total
  total <- dat %>% sel(nvar) %>% sfun(nvar, fun = fun)

  ## row and colum totals
  if (length(cvars) == 1) {
    tab <-
      bind_rows(
        tab,
        bind_cols(data.frame("Total") %>% setNames(cvars), total %>% set_colnames(nvar))
      )
  } else {

    col_total <-
      dat %>%
      group_by_(.dots = cvars[1]) %>%
      sel(nvar) %>%
      sfun(nvar, cvars[1], fun)

    row_total <-
      dat %>%
      group_by_(.dots = cvars[-1]) %>%
      sfun(nvar, cvars[-1], fun) %>%
      ungroup %>%
      select_(Total = nvar) %>%
      bind_rows(total %>% set_colnames("Total"))

    ## creating cross tab
    tab <- spread_(tab, cvars[1], nvar)
    tab <-
      bind_rows(
        tab,
        bind_cols(
          t(rep("Total",length(cvars[-1]))) %>% as.data.frame %>% setNames(cvars[-1]) ,
          data.frame(t(col_total[[2]])) %>% set_colnames(col_total[[1]])
        )
      ) %>% bind_cols(row_total)

      rm(col_total, row_total)
  }

  ## resetting factor levels
  ind <- ifelse(length(cvars) > 1, -1, 1)
  levs <- lapply(select_(dat, .dots = cvars[ind]), levels)
  for(i in cvars[ind])
    tab[[i]] <- factor(tab[[i]], levels = c(levs[[i]],"Total"))

  isNum <- -which(names(tab) %in% cvars)
  if (normalize == "total") {
    tab[,isNum] %<>% {. / total[[1]]} %>% round(3)
  } else if (normalize == "row") {
    if (!is.null(tab[["Total"]]))
      tab[,isNum] %<>% {. / select(., Total)[[1]]} %>% round(3)
  } else if (normalize == "column") {
    tab[,isNum] %<>% apply(2, function(.) . / .[which(tab[,1] == "Total")]) %>% round(3)
    ## mutate_each has issues for spaces in variable names
    # tab[,isNum] %<>% mutate_each_(funs(h = . / .[which(tab[,1] == "Total")]), vars = colnames(.)) %>% round(3)
  }

  if(!shiny) tab <- as.data.frame(tab, as.is = TRUE)

  rm(cv, isNum, dat, sfun, sel, i, levs, total, ind)

  environment() %>% as.list %>% set_class(c("pivotr",class(.)))
}

#' Summary method for pivotr
#'
#' @details See \url{http://vnijs.github.io/radiant/base/pivotr.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{pivotr}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' pivotr("diamonds", cvars = "cut") %>% summary
#' pivotr("diamonds", cvars = "cut") %>% summary
#' pivotr("diamonds", cvars = "cut:clarity", nvar = "price") %>% summary
#'
#' @seealso \code{\link{pivotr}} to create the pivot-table using dplyr
#'
#' @export
summary.pivotr <- function(object, ...) {

  cat("Pivot table\n")
  cat("Data       :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter     :", gsub("\\n","", object$data_filter), "\n")
  cat("Categorical:", object$cvars, "\n")

  if (object$nvar != "n") {
    cat("Numeric    :", object$nvar, "\n")
    cat("Function   :", object$fun, "\n")
  }
  cat("\n")

  print(object$tab, row.names = FALSE)
}

#' Make a pivot tabel in DT
#'
#' @details See \url{http://vnijs.github.io/radiant/base/pivotr.html} for an example in Radiant
#'
#' @param pvt Return value from \code{\link{pivotr}}
#' @param check Show "color_bar" and/or display numbers as percentages ("perc")
#'
#' @examples
#' pivotr("diamonds", cvars = "cut") %>% make_dt
#' pivotr("diamonds", cvars = c("cut","clarity")) %>% make_dt(color_bar = TRUE)
#'
#' @seealso \code{\link{pivotr}} to create the pivot-table using dplyr
#' @seealso \code{\link{summary.pivotr}} to print a plain table
#'
#' @export
make_dt <- function(pvt, check = "") {

  tab <- pvt$tab
  cvar <- pvt$cvars[1]
  cvars <- pvt$cvars %>% {if(length(.) > 1) .[-1] else .}
  cn <- colnames(tab) %>% {.[-which(cvars %in% .)]}

  tot <- tail(tab,1)[-(1:length(cvars))]
  if ("perc" %in% check)
    tot <- sprintf("%.2f%%", tot*100)
  else
    tot <- round(tot, 3)

  if(length(cvars) == 1 && cvar == cvars) {

    # if (perc)
    #   tf <- sprintf("%.2f%%", tail(tab,1)*100)
    # else
    #   tf <- round(tail(tab,1), 3)
    # sketch = htmltools::withTags(table(
    sketch = withTags(table(
      thead(
        tr(lapply(c(cvars,cn), th))
      ),
      tfoot(
        tr(lapply(c("Total",tot), th))
      )
    ))
  } else {
    # tot <- tot[-(1:length(cvars))]
    # if (perc)
    #   tf <- sprintf("%.2f%%", tot*100)
    # else
    #   tf <- round(tot, 3)

    # sketch = htmltools::withTags(table(
    sketch = withTags(table(
      thead(
        tr(
          # lapply(cvars, th),
          # th(colspan = length(cn), cvar, class = "text-center")
          th(colspan = length(c(cvars,cn)), cvar, class = "text-center")
        ),
        tr(
          # lapply(c(rep("",length(cvars)),cn), th)
          lapply(c(cvars,cn), th)
        )
      ),
      tfoot(
        tr(
          th(colspan = length(cvars), "Total"),
          lapply(tot, th)
        )
      )
    ))
  }

  ## remove column totals
  ## should perhaps be part of pivotr but convenient for for now in tfoot
  ## and for external calls to pivotr
  tab <- filter(tab, tab[,1] != "Total")

  # if (pvt$shiny) {
    # dt_fun <- DT::datatable
    # dt_fun <- DT::renderDataTable

  dt_tab <- tab %>%
  DT::datatable(container = sketch, filter = list(position = "top", clear = FALSE, plain = TRUE),
    rownames = FALSE, style = ifelse(pvt$shiny, "bootstrap", "default"),
    # style = "bootstrap",
    options = list(
      search = list(regex = TRUE),
      # autoWidth = TRUE,
      # columnDefs = list(list(className = 'dt-center', targets = "_all")),
      processing = FALSE,
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
    )
  ) %>% DT::formatStyle(., cvars,  color = "white", backgroundColor = "grey")

  if("color_bar" %in% check) {
    for(i in cn) {
      dt_tab %<>% DT::formatStyle(i,
          background = DT::styleColorBar(tab[[i]], 'lightblue'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center')
    }
  }

  if("perc" %in% check) {
    for(i in cn)
      dt_tab %<>% DT::formatPercentage(., i, 2)
  }

  dt_tab

  ## can use this in R > Report inside Radiant but doesn't export
  # renderDataTable({make_dt(result)})
}

# install.packages("radiant", repos = "http://vnijs.github.io/radiant_miniCRAN/")
# library(radiant)

# result <- pivotr("diamonds", cvars = "cut")
# result$shiny <- TRUE
# make_dt(result)

# DT::datatable(iris, style = 'bootstrap')

# library(DT)
# iris2 = head(iris, 20)
# options(DT.options = list(pageLength = 5))
# # default Bootstrap style in DT
# datatable(iris2, style = 'bootstrap')
