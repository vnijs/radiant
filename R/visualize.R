#' Visualize data using ggplot2 \url{http://docs.ggplot2.org/current/}
#'
#' @details See \url{http://vnijs.github.io/radiant/base/visualize.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param xvar One or more variables to display along the X-axis of the plot
#' @param yvar Variable to display along the Y-axis of the plot (default = "none")
#' @param comby Combine yvars in plot (TRUE or FALSE, FALSE is the default)
#' @param type Type of plot to create. One of Histogram ('hist'), Density ('density'), Scatter ('scatter'), Line ('line'), Bar ('bar'), or Box-plot ('box')
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different color
#' @param fill Group bar, histogram, and density plots by group, each with a different color
#' @param bins Number of bins used for a histogram (1 - 50)
#' @param smooth Adjust the flexibility of the loess line for scatter plots
#' @param sbar Plot an error bar in a scatter plot where the xvar is a factor. Options are "mean" and/or "median". Default is "mean"
#' @param check Add a regression line ("line"), a loess line ("loess"), or jitter ("jitter") to a scatter plot
#' @param axes Flip the axes in a plot ("flip") or apply a log transformation (base e) to the y-axis ("log_y") or the x-axis ("log_x")
#' @param alpha Opacity for plot elements (0 to 1)
#' @param data_filter Expression used to filter the dataset. This should be a string (e.g., "price > 10000")
#' @param shiny Logical (TRUE, FALSE) to indicate if the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#'
#' @return Generated plots
#'
#' @examples
#' visualize("diamonds", "carat", "price", type = "scatter", check = "loess")
#' visualize("diamonds", "price:x", type = "hist")
#' visualize("diamonds", "carat:x", yvar = "price", type = "scatter")
#' visualize(dataset = "diamonds", yvar = "price", xvar = "carat", type = "scatter", custom = TRUE) +
#'   ggtitle("A scatterplot") + xlab("price in $")
#' visualize(dataset = "diamonds", xvar = "price:carat", custom = TRUE) %>%
#'   {.[[1]] + ggtitle("A histogram") + xlab("price in $")}
#' diamonds %>% visualize(c("price","carat","depth"), type = "density")
#'
#' @export
visualize <- function(dataset, xvar,
                      yvar = "",
                      comby = FALSE,
                      type = "hist",
                      facet_row = ".",
                      facet_col = ".",
                      color = "none",
                      fill = "none",
                      bins = 10,
                      smooth = 1,
                      sbar = "mean",
                      check = "",
                      axes = "",
                      alpha = .5,
                      data_filter = "",
                      shiny = FALSE,
                      custom = FALSE) {

  ## inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
  vars <- xvar

  if (!type %in% c("scatter","line")) color <- "none"
  if (type != "scatter") check %<>% sub("line","",.) %>% sub("loess","",.)
  if (!type %in% c("scatter","box")) check %<>% sub("jitter","",.)
  if (!type %in% c("scatter","line")) color <- "none"
  if (!type %in% c("bar","hist","density")) fill <- "none"

  ## variable to use if bar chart is specified
  byvar <- NULL

  if (length(yvar) == 0 || identical(yvar, "")) {
    if (!type %in% c("hist","density")) {
      return("No Y-variable provided for a plot that requires one")
    }
  } else {
    if (type %in% c("hist","density")) {
      yvar <- ""
    } else {
      vars %<>% c(., yvar)
    }
  }

  if (color != "none") {
    vars %<>% c(., color)
    if (type == "line") byvar <- color
  }
  if (facet_row != ".") {
    vars %<>% c(., facet_row)
    byvar <- if (is.null(byvar)) facet_row else unique(c(byvar, facet_row))
  }
  if (facet_col != ".") {
    vars %<>% c(., facet_col)
    byvar <- if (is.null(byvar)) facet_col else unique(c(byvar, facet_col))
  }
  if (fill != "none") {
    vars %<>% c(., fill)
    if (type == "bar")
      byvar <- if (is.null(byvar)) fill else unique(c(byvar, fill))
  }

  ## so you can also pass-in a data.frame
  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  ## if : is used to specify a range of variables
  if (length(vars) < ncol(dat)) {
    fl <- strsplit(xvar,":") %>% unlist
    cn <- colnames(dat)
    xvar <- cn[which(fl[1] == cn):which(fl[2] == cn)]
  }

  ## convertising factor variables if needed
  isChar <- sapply(dat, class) == "character"
  if (sum(isChar) > 0) {
    if (type == "density") {
      dat[,isChar] <- select(dat, which(isChar)) %>% mutate_each(funs(as_numeric))
      if ("character" %in% getclass(select(dat,which(isChar))))
        return("Character variable(s) were not converted to numeric.\nTo use these variables in a plot convert them to numeric\nvariables (or factors) in the Data > Transform tab")
    } else {
      dat[,isChar] <- select(dat, which(isChar)) %>% mutate_each(funs(as_factor))
      nrlev <- sapply(dat, function(x) if (is.factor(x)) length(levels(x)) else 0)
      if (max(nrlev) > 50)
        return("Character variable(s) were not converted to factors.\nTo use these variable in a plot convert them to factors\n(or numeric variables) in the Data > Transform tab")
    }
  }

  if (xor("log_x" %in% axes, "log_y" %in% axes)) {
    if (any(xvar %in% yvar))
      return("When applying 'Log X' an X-variable cannot also be selected as a Y-variable")
    if (any(yvar %in% xvar))
      return("When applying 'Log Y' a Y-variable cannot also be selected as an X-variable")
  }

  log_trans <- function(x) ifelse(x > 0, log(x), NA)

  if ("log_x" %in% axes) {
    to_log <- (getclass(select_(dat, .dots = xvar)) %in% c("integer","numeric")) %>% xvar[.]
    dat[, to_log] <- select_(dat, .dots = to_log) %>% mutate_each(funs(log_trans))
  }

  if ("log_y" %in% axes) {
    to_log <- (getclass(select_(dat, .dots = yvar)) %in% c("integer","numeric")) %>% yvar[.]
    dat[, to_log] <- select_(dat, .dots = to_log) %>% mutate_each(funs(log_trans))
  }

  ## combining yvariables if needed
  if (comby && length(yvar) > 1) {
    if (any(xvar %in% yvar)) return("X-variables cannot be part of Y-variables when combining Y-variables")
    if (!is_empty(color, "none")) return("Cannot use Color when combining Y-variables")
    if (!is_empty(fill, "none")) return("Cannot use Fill when combining Y-variables")

    dat <- gather_(dat, "yvar", "values", gather_cols = yvar)
    yvar <- "values"
    byvar <- if (is.null(byvar)) "yvar" else c("yvar", byvar)
    color <- fill <- "yvar"
  }

  plot_list <- list()
  if (type == "hist") {
    for (i in xvar) {

      ## can't create a histogram for a logical
      if ("logical" %in% class(dat[[i]])) dat[[i]] <- as_factor(dat[[i]])

      hist_par <- list(alpha = alpha, position = "dodge")
      plot_list[[i]] <- ggplot(dat, aes_string(x=i))
      if ("density" %in% axes) {
        hist_par <- list(aes(y = ..density..), alpha = alpha, position = "dodge")
        plot_list[[i]] <- plot_list[[i]] + geom_density(color = "blue", size = .5)
      }
      if (!"factor" %in% class(dat[[i]])) {
        # if ("log_x" %in% axes)
        #   hist_par[["binwidth"]] <- select_(dat,i) %>% filter(. > 0) %>% mutate_each(funs(log)) %>% range %>% {diff(.)/bins}
        # else
        hist_par[["binwidth"]] <- select_(dat,i) %>% range %>% {diff(.)/bins}
      } else {
        if ("log_x" %in% axes) axes <- sub("log_x","",axes)
      }

      plot_list[[i]] <- plot_list[[i]] + do.call(geom_histogram, hist_par)
      if ("log_x" %in% axes) plot_list[[i]] <- plot_list[[i]] + xlab(paste("log", i))
    }
  } else if (type == "density") {
    for (i in xvar) {
      plot_list[[i]] <- ggplot(dat, aes_string(x=i)) +
        if (fill == "none")
          geom_density(adjust = smooth, color = "blue", fill = "blue", alpha = alpha, size = 1)
        else
          geom_density(adjust = smooth, alpha = alpha, size = 1)

      if ("log_x" %in% axes) plot_list[[i]] <- plot_list[[i]] + xlab(paste("log", i))
    }
  } else if (type == "scatter") {

    itt <- 1
    if ("jitter" %in% check) {
      # gs <- geom_blank() + geom_jitter(alpha = alpha, position = position_jitter(width = 0.4, height = 0.1))
      gs <- geom_jitter(alpha = alpha, position = position_jitter(width = 0.4, height = 0.1))
      check <- sub("jitter","", check)
    } else {
      gs <- geom_point(alpha = alpha)
    }

    for (i in xvar) {
      if ("log_x" %in% axes && "factor" %in% class(dat[[i]])) axes <- sub("log_x","",axes)
      for (j in yvar) {
        plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + gs

        if ("log_x" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))

        if ("factor" %in% class(dat[[i]])) {

          ## make range comparable to bar plot
          ymax <- max(dat[[j]]) %>% {if (. < 0) 0 else .}
          ymin <- min(dat[[j]]) %>% {if (. > 0) 0 else .}
          plot_list[[itt]] <- plot_list[[itt]] + ylim(ymin,ymax)


          if ("mean" %in% sbar) {
            plot_list[[itt]] <- plot_list[[itt]] +
              geom_errorbar(stat = "hline", yintercept = "mean", width = .8, size = 1, color = "blue", aes(ymax = ..y.., ymin = ..y..))
          }

          if ("median" %in% sbar) {
            plot_list[[itt]] <- plot_list[[itt]] +
            geom_errorbar(stat = "hline", yintercept = "median", width = .8, size = 1, color = "red", aes(ymax = ..y.., ymin = ..y..))
          }
        }

        itt <- itt + 1
      }
    }
  } else if (type == "line") {
    itt <- 1
    for (i in xvar) {
      for (j in yvar) {
        if (color == 'none') {
          if (is.factor(dat[[i]])) {
            tbv <- if (is.null(byvar)) i else c(i, byvar)
            tmp <- dat %>% group_by_(.dots = tbv) %>% select_(j) %>% summarise_each(funs(mean))
            plot_list[[itt]] <- ggplot(tmp, aes_string(x=i, y=j)) + geom_point() + geom_line(aes(group = 1))
          } else {
            plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + geom_line()
          }
        } else {
          if (is.factor(dat[[i]])) {
            tbv <- if (is.null(byvar)) i else c(i, byvar)
            tmp <- dat %>% group_by_(.dots = tbv) %>% select_(j, color) %>% summarise_each(funs(mean))
            # plot_list[[itt]] <- ggplot(tmp, aes_string(x=i, y=j, color = color)) + geom_point() + geom_line(aes(group = 1))
            plot_list[[itt]] <- ggplot(tmp, aes_string(x=i, y=j, color = color, group = color)) + geom_point() + geom_line()
          } else {
            # plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j, color = color)) + geom_line()
            plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j, color = color, group = color)) + geom_line()
          }
        }
        if ("log_x" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))
        itt <- itt + 1
      }
    }
  } else if (type == "bar") {
    itt <- 1
    for (i in xvar) {
      dat[,i] %<>% as_factor
      if ("log_x" %in% axes) axes <- sub("log_x","",axes)
      for (j in yvar) {
        # if ("log_y" %in% axes && i == j) axes <- sub("log_y","",axes)

        tbv <- if (is.null(byvar)) i else c(i, byvar)
        tmp <- dat %>% group_by_(.dots = tbv) %>% select_(j) %>% summarise_each(funs(mean))

        if ("sort" %in% axes && facet_row == "." && facet_col == ".") {
          tmp <- arrange_(ungroup(tmp), j)
          tmp[[i]] %<>% factor(., levels = unique(.))
        }

        plot_list[[itt]] <- ggplot(tmp, aes_string(x=i, y=j)) +
          geom_bar(stat="identity", position = "dodge", alpha = alpha)

        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))

        itt <- itt + 1
      }
    }
  } else if (type == "box") {
    itt <- 1
    for (i in xvar) {
      dat[,i] %<>% as_factor
      for (j in yvar) {
        # if ("log_y" %in% axes && i == j) axes <- sub("log_y","",axes)
        plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j, fill=i)) +
                          geom_boxplot(alpha = alpha) +
                          theme(legend.position = "none")

        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))

        itt <- itt + 1
      }
    }
  }

  if (facet_row != "." || facet_col != ".") {
    facets <- if (facet_row == ".")  paste("~", facet_col)
              else paste(facet_row, '~', facet_col)
    scales <- if ("scale_y" %in% axes) "free_y" else "fixed"
    facet_fun <- if (facet_row == ".") facet_wrap else facet_grid
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + facet_fun(as.formula(facets), scales = scales)

  }

  if (color != 'none') {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + aes_string(color=color)
  }

  ## adding fill
  if (fill != 'none') {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + aes_string(fill = fill)
  }

  if ("jitter" %in% check) {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] +
        geom_jitter(alpha = alpha, position = position_jitter(width = 0.4, height = 0.1))
  }

  if ("line" %in% check) {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + sshhr( geom_smooth(method = "lm", fill = 'blue',
                                             alpha = .1, size = .75,
                                             linetype = "dashed",
                                             colour = 'black') )
  }

  if ("loess" %in% check) {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + sshhr( geom_smooth(span = smooth, method = "loess", size = .75,
                                             linetype = "dotdash", aes(group=1)) )
  }

  if ("flip" %in% axes) {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + coord_flip()
  }

  # if ("log_y" %in% axes) {
  #   for (i in 1:length(plot_list))
  #     plot_list[[i]] <- plot_list[[i]] + scale_y_continuous(trans = "log")
  # }

  # if ("log_x" %in% axes) {
  #   for (i in 1:length(plot_list))
  #     plot_list[[i]] <- plot_list[[i]] + scale_x_continuous(trans = "log")
  # }

 if (custom)
   if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)


 sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = min(length(plot_list), 2)))) ) %>%
   { if (shiny) . else print(.) }
}
