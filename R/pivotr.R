#' Create a pivot table using dplyr
#'
#' @details Create a pivot-table. See \url{http://vnijs.github.io/radiant/base/pivotr.html} for an example in Radiant
#'
#' @param dataset Name of the dataframe to change
#' @param cvars Categorical variables
#' @param nvar Numerical variable
#' @param fun Function to apply to numerical variable
#' @param normalize Normalize the table by "row" total,"column" totals, or overall "total"
#' @param tabfilt Expression used to filter the table. This should be a string (e.g., "Total > 10000")
#' @param tabsort Expression used to sort the table (e.g., "-Total")
#' @param data_filter Expression used to filter the dataset. This should be a string (e.g., "price > 10000")
#' @param shiny Logical (TRUE, FALSE) to indicate if the function call originate inside a shiny app
#'
#' @examples
#' result <- pivotr("diamonds", cvars = "cut")$tab
#' result <- pivotr("diamonds", cvars = c("cut","clarity","color"))$tab
#' result <- pivotr("diamonds", cvars = "cut:clarity", nvar = "price")$tab
#' result <- pivotr("diamonds", cvars = "cut", normalize = "total")$tab
#'
#' @export
pivotr <- function(dataset,
                   cvars = "",
                   nvar = "None",
                   fun = "mean_rm",
                   normalize = "None",
                   tabfilt = "",
                   tabsort = "",
                   data_filter = "",
                   shiny = FALSE) {


  vars <- if (nvar == "None") cvars else c(cvars, nvar)
  dat <- getdata(dataset, vars, filt = data_filter, na.rm = FALSE)
  if (!is_string(dataset)) dataset <- "-----"

  ## in case : was used vor cvars
  if (length(vars) < ncol(dat))
    cvars <- colnames(dat) %>% {.[. != nvar]}

  if (nvar == "None") {
    nvar <- "n"
  } else {
    ## converting factors for interger (1st level)
    ## see also R/visualize.R
    if ("factor" %in% class(dat[[nvar]]))
      dat[[nvar]] %<>% {as.integer(. == levels(.)[1])}
    if ("logical" %in% class(dat[[nvar]]))
      dat[[nvar]] %<>% as.integer
  }

  ## convert categorical variables to factors if needed
  ## needed to deal with empty/missing values
  empty_level <- function(x) {
    if (!is.factor(x)) x %<>% as.factor
    levs <- levels(x)
    if ("" %in% levs) {
      levs[levs == ""] <- "NA"
      x <- factor(x, levels = levs)
      x[is.na(x)] <- "NA"
    } else if (any(is.na(x))) {
      x <- factor(x, levels = c(levs,"NA"))
      x[is.na(x)] <- "NA"
    }
    x
  }

  dat[,cvars] <- select_(dat, .dots = cvars) %>% mutate_each(funs(empty_level(.)))

  sel <- function(x, nvar) if (nvar == "n") x else select_(x, .dots = nvar)
  sfun <- function(x, nvar, cvars = "", fun = fun) {
    if (nvar == "n")
      if (all(cvars == "")) count_(x) else count_(x, cvars)
    else
      mutate_each_(x, "as.numeric", vars = nvar) %>%
      summarise_each_(make_funs(fun), vars = nvar)
  }

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

  # tab %>% getclass
  ## resetting factor levels
  ind <- ifelse (length(cvars) > 1, -1, 1)
  levs <- lapply(select_(dat, .dots = cvars[ind]), levels)

  for (i in cvars[ind])
    tab[[i]] %<>% factor(., levels = c(levs[[i]],"Total"))

  ## frequency table for chi-square test
  tab_freq <- tab

  isNum <- -which(names(tab) %in% cvars)
  if (normalize == "total") {
    tab[,isNum] %<>% {. / total[[1]]} #%>% round(dec)
  } else if (normalize == "row") {
    if (!is.null(tab[["Total"]]))
      tab[,isNum] %<>% {. / select(., Total)[[1]]} #%>% round(dec)
  } else if (length(cvars) > 1 && normalize == "column") {
    tab[,isNum] %<>% apply(2, function(.) . / .[which(tab[,1] == "Total")]) # %>% round(dec)
    ## mutate_each has issues for spaces in variable names
    # tab[,isNum] %<>% mutate_each_(funs(h = . / .[which(tab[,1] == "Total")]), vars = colnames(.)) %>% round(3)
  }

  ## filtering the table if desired
  if (tabfilt != "")
    tab <- filterdata(tab, tabfilt) %>% droplevels

  ## sorting the table if desired
  if (!identical(tabsort, "")) {

    if (grepl(",", tabsort))
      tabsort <- strsplit(tabsort,",")[[1]] %>% gsub(" ", "", .)
    tab[-nrow(tab),] %<>% arrange_(.dots = tabsort)

    # for (i in cvars)
    #   tab[[i]] %<>% factor(., levels = unique(.))
  }

  if (!shiny) tab <- as.data.frame(tab, as.is = TRUE)

  rm(isNum, dat, sfun, sel, i, levs, total, ind)

  environment() %>% as.list %>% set_class(c("pivotr",class(.)))
}

#' Summary method for pivotr
#'
#' @details See \url{http://vnijs.github.io/radiant/base/pivotr.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{pivotr}}
#' @param perc Display numbers as percentages (TRUE or FALSE)
#' @param dec Number of decimals to show
#' @param chi2 If TRUE calculate the chi-square statistic for the (pivot) table
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' pivotr("diamonds", cvars = "cut") %>% summary
#' pivotr("diamonds", cvars = "cut", tabsort = "-n") %>% summary
#' pivotr("diamonds", cvars = "cut", tabfilt = "n > 700") %>% summary
#' pivotr("diamonds", cvars = "cut:clarity", nvar = "price") %>% summary
#'
#' @seealso \code{\link{pivotr}} to create the pivot-table using dplyr
#'
#' @export
summary.pivotr <- function(object,
                           perc = FALSE,
                           dec = 3,
                           chi2 = FALSE,
                           shiny = FALSE,
                           ...) {

  if (!shiny) {
    cat("Pivot table\n")
    cat("Data        :", object$dataset, "\n")
    if (object$data_filter %>% gsub("\\s","",.) != "")
      cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
    cat("Categorical :", object$cvars, "\n")

    if (object$normalize != "None")
      cat("Normalize by:", object$normalize, "\n")

    if (object$nvar != "n") {
      cat("Numeric     :", object$nvar, "\n")
      cat("Function    :", object$fun, "\n")
    }
    cat("\n")
    print(dfprint(object$tab, dec, perc), row.names = FALSE)
    cat("\n")
  }

  if (chi2) {
    cst <- object$tab_freq %>% filter(.[[1]] != "Total") %>%
      select(-which(names(.) %in% c(object$cvars, "Total")))  %>%
      mutate_each(funs(rep_na = ifelse (is.na(.),0,.))) %>%
      {sshhr(chisq.test(., correct = FALSE))}

    # res <- cst %>% tidy %>% {if (.$p.value < .001) .$p.value <- 0; .} %>% round(dec)
    res <- cst %>% tidy %>% round(dec)

    if (dec < 4)
      if (res$p.value < .001) res$p.value  <- "< .001"
    else
      res$p.value  <- nrprint(res$p.value, dec = dec)

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
#' @param dec Number of decimals to show
#' @param search Global search. Used to save and restore state
#' @param searchCols Column search and filter. Used to save and restore state
#' @param order Column sorting. Used to save and restore state
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
make_dt <- function(pvt,
                    format = "none",
                    perc = FALSE,
                    dec = 3,
                    search = "",
                    searchCols = NULL,
                    order = NULL) {

  tab <- pvt$tab
  cvar <- pvt$cvars[1]
  cvars <- pvt$cvars %>% {if (length(.) > 1) .[-1] else .}
  cn <- colnames(tab) %>% {.[-which(cvars %in% .)]}

  ## column names without total
  cn_nt <- if ("Total" %in% cn) cn[-which(cn == "Total")] else cn

  tot <- tail(tab,1)[-(1:length(cvars))]
  if (perc)
    # tot <- nrprint(tot * 100, dec = dec, perc = perc)
    tot <- sprintf(paste0("%.", dec ,"f%%"), tot * 100)
  else
    tot <- round(tot, dec)

  if (length(cvars) == 1 && cvar == cvars) {
    sketch = shiny::withTags(table(
      thead(tr(lapply(c(cvars,cn), th))),
      tfoot(tr(lapply(c("Total",tot), th)))
    ))
  } else {
    sketch = shiny::withTags(table(
      thead(
        tr(th(colspan = length(c(cvars,cn)), cvar, class = "text-center")),
        tr(lapply(c(cvars,cn), th))
      ),
      tfoot(
        tr(th(colspan = length(cvars), "Total"), lapply(tot, th))
      )
    ))
  }

  ## remove column totals
  ## should perhaps be part of pivotr but convenient for now in tfoot
  ## and for external calls to pivotr
  tab <- filter(tab, tab[,1] != "Total")

  if (nrow(tab) > 5000000) {
    fbox <- "none"
  } else {
    fbox <- list(position = "top")
    dc <- getclass(tab)
    if ("factor" %in% dc) {
      toChar <- sapply(select(tab, which(dc == "factor")), function(x) length(levels(x))) > 100
      if (any(toChar))
        tab <- mutate_each_(tab, funs(as.character), vars = names(toChar)[toChar])
    }
  }

  dt_tab <- tab %>% {if (!perc) dfround(.,dec) else .} %>%
  DT::datatable(container = sketch, selection = "none", rownames = FALSE,
    filter = fbox,
    # style = ifelse (pvt$shiny, "bootstrap", "default"),
    style = "bootstrap",
    options = list(
      stateSave = TRUE,
      search = list(search = search, regex = TRUE),
      searchCols = searchCols,
      order = order,
      processing = FALSE,
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","All"))
    )
    , callback = DT::JS("$(window).unload(function() { table.state.clear(); })")
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
    ## round seems to ensure that 'cuts' are ordered according to DT::stylInterval
    brks <- quantile(tab[, cn_nt], probs = seq(.05, .95, .05), na.rm = TRUE) %>% round(5)
    clrs <- seq(255, 40, length.out = length(brks) + 1) %>%
      round(0) %>%
      {paste0("rgb(255,", ., ",", .,")")}

    dt_tab %<>% DT::formatStyle(cn_nt, backgroundColor = DT::styleInterval(brks, clrs))
  }

  ## show percentage
  if (perc) dt_tab %<>% DT::formatPercentage(cn, dec)

  ## see https://github.com/yihui/knitr/issues/1198
  dt_tab$dependencies <- c(
    list(rmarkdown::html_dependency_bootstrap('bootstrap')), dt_tab$dependencies
  )

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
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' pivotr("diamonds", cvars = "cut") %>% plot
#' pivotr("diamonds", cvars = c("cut","clarity")) %>% plot
#' pivotr("diamonds", cvars = c("cut","clarity","color")) %>% plot
#'
#' @seealso \code{\link{pivotr}} to generate summaries
#' @seealso \code{\link{summary.pivotr}} to show summaries
#'
#' @export
plot.pivotr <- function(x,
                        type = "dodge",
                        perc = FALSE,
                        flip = FALSE,
                        shiny = FALSE,
                        custom = FALSE,
                        ...) {

  object <- x; rm(x)
  cvars <- object$cvars
  nvar <- object$nvar
  tab <- object$tab %>% {filter(., .[[1]] != "Total")}
  plot_list <- list()

  if (length(cvars) == 1) {
    plot_list[[1]] <-
      ggplot(na.omit(tab), aes_string(x = cvars, y = nvar)) +
        geom_bar(stat="identity", position = "dodge", alpha=.7)
  } else if (length(cvars) == 2) {
    ctot <- which(colnames(tab) == "Total")
    if (length(ctot) > 0) tab %<>% select(-matches("Total"))

    # dots <- paste0(cvars[1]," = factor(",cvars[1],", levels = c('", paste0(setdiff(colnames(tab),cvars[2]),collapse="','"),"'))")
    dots <- paste0("factor(",cvars[1],", levels = c('", paste0(setdiff(colnames(tab),cvars[2]),collapse="','"),"'))")
    plot_list[[1]] <-
      tab %>% gather_(cvars[1], nvar, setdiff(colnames(.),cvars[2])) %>% na.omit %>%
        mutate_(.dots = setNames(dots,cvars[1])) %>%
        ggplot(aes_string(x = cvars[1], y = nvar, fill = cvars[2])) +
          geom_bar(stat="identity", position = type, alpha=.7)
  } else if (length(cvars) == 3) {
    ctot <- which(colnames(tab) == "Total")
    if (length(ctot) > 0) tab %<>% select(-matches("Total"))

    # dots <- paste0(cvars[1]," = factor(",cvars[1],", levels = c('", paste0(setdiff(colnames(tab),cvars[2:3]),collapse="','"),"'))")
    dots <- paste0("factor(",cvars[1],", levels = c('", paste0(setdiff(colnames(tab),cvars[2:3]),collapse="','"),"'))")
    plot_list[[1]] <-
      tab %>% gather_(cvars[1], nvar, setdiff(colnames(.),cvars[2:3])) %>% na.omit %>%
        mutate_(.dots = setNames(dots,cvars[1])) %>%
        ggplot(aes_string(x = cvars[1], y = nvar, fill = cvars[2])) +
          geom_bar(stat="identity", position = type, alpha=.7) +
          facet_grid(paste(cvars[3], '~ .'))
  } else {
    ## You are pushing this feature a bit too far dude
    return(invisible())
  }

  if (flip) plot_list[[1]] <- plot_list[[1]] + coord_flip()
  if (perc) plot_list[[1]] <- plot_list[[1]] + scale_y_continuous(labels = scales::percent)
  if (nvar == "n") {
    if (!is_empty(object$normalize, "None"))
      plot_list[[1]] <- plot_list[[1]] + ylab(ifelse (perc, "Percentage", "Proportion"))
  } else {
    plot_list[[1]] <- plot_list[[1]] + ylab(paste0(nvar, " (",names(make_funs(object$fun)),")"))
  }

 if (custom)
   if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

  # sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
  sshhr( plot_list[[1]] ) %>%
    { if (shiny) . else print(.) }
}
