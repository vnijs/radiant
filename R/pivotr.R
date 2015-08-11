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
#' result <- pivotr("diamonds", cvars = "cut")$tab
#' result <- pivotr("diamonds", cvars = c("cut","clarity","color"))$tab
#' result <- pivotr("diamonds", cvars = "cut:clarity", nvar = "price")$tab
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

  ## use loop or mutate_each?
  for (cv in cvars)
    if (!is.factor(dat[[cv]])) dat[[cv]] %<>% as.factor

  ## convert categorical variables to factors if needed
  ## using [] seems weird but drop = FALSE doesn't wor
  # dat[cvars] %<>% mutate_each(funs({if(is.factor(.)) . else as.factor(.)}))

  # ind <- ifelse(length(cvars) > 1, -1, 1)
  # levs <- lapply(select_(dat, .dots = cvars[ind]), levels)

  # for (l in names(levs)) {
  #   if ("" %in% levs[[l]]) {
  #       dat[[l]] <- mutate_(dat, l = ifelse(l,"","MISSING"))[[l]]
  #       dat[[l]] <- factor(dat[[l]], levels = sub("^$","MISSING",levs[[l]]))
  #   }
  # }

  sel <- function(x, nvar) if (nvar == "n") x else select_(x, .dots = nvar)
  sfun <- function(x, nvar, cvars = "", fun = fun) {
    if (nvar == "n")
      if (all(cvars == "")) count_(x) else count_(x, cvars)
    else
      mutate_each_(x, "as.numeric", vars = nvar) %>%
      summarise_each_(make_funs(fun), vars = nvar)
  }
  # levs <- lapply(select_(dat, .dots = cvars[ind]), levels)

  ## main tab
  tab <-
    dat %>%
    group_by_(.dots = cvars) %>%
    sfun(nvar, cvars, fun)

  ## total
  total <-
    dat %>% sel(nvar) %>% sfun(nvar, fun = fun)

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
  for (i in cvars[ind])
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

  if (!shiny) tab <- as.data.frame(tab, as.is = TRUE)

  rm(cv, isNum, dat, sfun, sel, i, levs, total, ind)

  environment() %>% as.list %>% set_class(c("pivotr",class(.)))
}

#' Summary method for pivotr
#'
#' @details See \url{http://vnijs.github.io/radiant/base/pivotr.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{pivotr}}
#' @param chi2 If TRUE calculate the chi-square statistic for the (pivot) table
#' @param shiny Did the function call originate inside a shiny app
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
summary.pivotr <- function(object, chi2 = FALSE, shiny = FALSE,  ...) {

  if (!shiny) {
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
    cat("\n")
  }

  if (chi2) {
    cst <- object$tab %>% filter(.[[1]] != "Total") %>%
      select(-which(names(.) %in% c(object$cvars, "Total")))  %>%
      mutate_each(funs(rep_na = ifelse(is.na(.),0,.))) %>%
      {sshhr(chisq.test(., correct = FALSE))}

    res <- cst %>% tidy %>% {if (.$p.value < .001) .$p.value <- 0; .} %>% round(3)
    if (res$p.value < .001) res$p.value  <- "< .001"
    l1 <- paste0("Chi-squared: ", res$statistic, " df(", res$parameter, "), p.value ", res$p.value, "\n")
    l2 <- paste0(sprintf("%.1f",100 * (sum(cst$expected < 5) / length(cst$expected))),"% of cells have expected values below 5")
    if (shiny) paste0("</br><hr>", l1, "</br>", l2) %>% HTML else cat(l1,l2)
  }
}

#' Make a pivot tabel in DT
#'
#' @details See \url{http://vnijs.github.io/radiant/base/pivotr.html} for an example in Radiant
#'
#' @param pvt Return value from \code{\link{pivotr}}
#' @param format Show Color bar ("color_bar"),  Heat map ("heat"), or None ("none")
#' @param perc Display numbers as percentages (TRUE or FALSE)
#'
#' @examples
#' pivotr("diamonds", cvars = "cut") %>% make_dt
#' pivotr("diamonds", cvars = c("cut","clarity")) %>% make_dt(format = "color_bar")
#' ret <-  pivotr("diamonds", cvars = c("cut","clarity"), normalize = "total") %>%
#'    make_dt(format = "color_bar", perc = TRUE)
#'
#' @seealso \code{\link{pivotr}} to create the pivot-table using dplyr
#' @seealso \code{\link{summary.pivotr}} to print a plain text table
#'
#' @export
make_dt <- function(pvt, format = "none", perc = FALSE) {

  tab <- pvt$tab
  cvar <- pvt$cvars[1]
  cvars <- pvt$cvars %>% {if(length(.) > 1) .[-1] else .}
  cn <- colnames(tab) %>% {.[-which(cvars %in% .)]}

  #############################################################
  ## work-around for https://github.com/rstudio/DT/issues/150
  tab[,cn] <- tab[,cn] %>% round(3)
  #############################################################

  ## column names without total
  cn_nt <- if ("Total" %in% cn) cn[-which(cn == "Total")] else cn

  tot <- tail(tab,1)[-(1:length(cvars))]
  if (perc)
    tot <- sprintf("%.2f%%", tot*100)
  else
    tot <- round(tot, 3)

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
  ## should perhaps be part of pivotr but convenient for now in tfoot
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
  if (perc) dt_tab %<>% DT::formatPercentage(cn, 2)

  dt_tab

  ## can use this in R > Report inside Radiant but doesn't export
  # renderDataTable({make_dt(result)})
}

#' Plot method for the pivotr function
#'
#' @details See \url{http://vnijs.github.io/radiant/base/pivotr} for an example in Radiant
#'
#' @param x Return value from \code{\link{pivotr}}
#' @param type Plot type to use ("fill" or "dodge" (default))
#' @param perc Use percentage on the y-axis
#' @param flip Flip the axes in a plot (FALSE or TRUE)
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{pivotr}} to generate summaries
#' @seealso \code{\link{summary.pivotr}} to show summaries
#'
#' @importFrom scales percent
#'
#' @export
plot.pivotr <- function(x, type = "dodge", perc = FALSE, flip = FALSE, shiny = FALSE, ...) {

  object <- x; rm(x)
  # object <- pivotr("diamonds", cvars = "cut", nvar = "price")
  # object <- pivotr("diamonds", cvars = c("cut","clarity"))
  # object <- pivotr("diamonds", cvars = c("cut","clarity"), nvar = "price")
  # object <- pivotr("diamonds", cvars = "cut", nvar = "price")
  # object <- pivotr("diamonds", cvars = c("cut","clarity","color"))
  # print(lubridate::now())

  cvars <- object$cvars
  nvar <- object$nvar
  tab <- object$tab %>% {filter(., .[[1]] != "Total")}
  plot_list <- list()

  if (length(cvars) == 1) {
    plot_list[[1]] <-
      ggplot(tab %>% na.omit, aes_string(x = cvars, y = nvar)) +
        geom_bar(stat="identity", position = "dodge", alpha=.7)
  } else if (length(cvars) == 2) {
    ctot <- which(colnames(tab) == "Total")
    if(length(ctot) > 0) tab %<>% select(-matches("Total"))

    plot_list[[1]] <-
      ggplot(tab %>% gather_(cvars[1], nvar) %>% na.omit, aes_string(x = cvars[1], y = nvar, fill = cvars[2])) +
        geom_bar(stat="identity", position = type, alpha=.7)
  } else if (length(cvars) == 3) {
    ctot <- which(colnames(tab) == "Total")
    if(length(ctot) > 0) tab %<>% select(-matches("Total"))

    plot_list[[1]] <-
      ggplot(tab %>% gather_(cvars[1], nvar) %>% na.omit, aes_string(x = cvars[1], y = nvar, fill = cvars[2])) +
        geom_bar(stat="identity", position = type, alpha=.7) +
        facet_grid(paste(cvars[3], '~ .'))
  } else {
    ## You are pushing this feature a bit too far dude
    return(invisible())
  }

  if (flip) plot_list[[1]] <- plot_list[[1]] + coord_flip()
  if (perc) plot_list[[1]] <- plot_list[[1]] + scale_y_continuous(labels = percent)

  sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
    { if (shiny) . else print(.) }
}

## client-side with bootstrap not working yet https://github.com/rstudio/DT/issues/143
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

## create tab for issue https://github.com/rstudio/DT/issues/150
# library(radiant)
# dat <- pivotr("diamonds", cvars = c("clarity","cut"), normalize = "total")$tab %>% filter(cut != "Total")
# dput(dat)
# DT::datatable(dat, filter = list(position = "top", clear = FALSE))

## if you need to recreate
# library(radiant)
# library(DT)
# dat <- pivotr("diamonds", cvars = "clarity", nvar = "price")$tab %>% filter(clarity != "Total")
# dput(dat)

# ## demo
# dat <- structure(list(clarity = structure(1:8, .Label = c("I1", "SI2",
# "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF", "Total"), class = "factor"),
#     price = c(4194.775, 5100.18903591682, 3998.57697642164, 3822.96671709531,
#     3789.18099547511, 3337.82042253521, 2608.45982142857, 2411.69696969697
#     )), class = "data.frame", row.names = c(NA, -8L), .Names = c("clarity",
# "price"))

# dat[,-1] <- round(dat[,-1], 3)

# datatable(dat, filter = list(position = "top", clear = FALSE))
