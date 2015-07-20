#' Visualize data using ggplot2 \url{http://docs.ggplot2.org/current/}
#'
#' @details See \url{http://vnijs.github.io/radiant/base/visualize.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param xvar One or more variables to display along the X-axis of the plot
#' @param yvar Variable to display along the Y-axis of the plot (default = "none")
#' @param type Type of plot to create. One of Histogram ('hist'), Density ('density'), Scatter ('scatter'), Line ('line'), Bar ('bar'), or Box-plot ('box')
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different colour
#' @param bins Number of bins used for a histogram (1 - 50)
#' @param smooth Adjust the flexibility of the loess line for scatter plots (not accessible in Radiant)
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
                      yvar = "none",
                      type = "hist",
                      facet_row = ".",
                      facet_col = ".",
                      color = "none",
                      bins = 10,
                      smooth = 1,
                      check = "",
                      axes = "",
                      alpha = .5,
                      data_filter = "",
                      shiny = FALSE,
                      custom = FALSE) {

  ## inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
  vars <- xvar

  if (!type %in% c("scatter","line")) color = "none"

  if (yvar != "none") vars %<>% c(., yvar)
  if (color != "none") vars %<>% c(., color)
  if (facet_row != ".") vars %<>% c(., facet_row)
  if (facet_col != ".") vars %<>% c(., facet_col)

  ## so you can also pass-in a data.frame
  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  ## if : is used to specify a range of variables
  if (length(vars) < ncol(dat)) {
    fl <- strsplit(xvar,":") %>% unlist
    cn <- colnames(dat)
    xvar <- cn[which(fl[1] == cn):which(fl[2] == cn)]
  }

  isChar <- sapply(dat, class) == "character"
  if (sum(isChar) > 0) {
    if (type == "density")
      dat[,isChar] %<>% data.frame %>% mutate_each(funs(as.numeric))
    else
      dat[,isChar] %<>% data.frame %>% mutate_each(funs(as.factor))
  }

  plot_list <- list()

  if (type == "hist") {
    for (i in xvar) {
      hist_par <- list(alpha = alpha)
      plot_list[[i]] <- ggplot(dat, aes_string(x=i))
      if ("density" %in% axes) {
        hist_par <- list(aes(y = ..density..), alpha = alpha)
        plot_list[[i]] <- plot_list[[i]] + geom_density(color = "blue", size = .5)
      }
      if (!"factor" %in% class(dat[[i]]))
        hist_par[["binwidth"]] <- select_(dat,i) %>% range %>% diff(.)/bins

      plot_list[[i]] <- plot_list[[i]] + do.call(geom_histogram, hist_par)
    }
  } else if (type == "density") {
    for (i in xvar) {
      plot_list[[i]] <- ggplot(dat, aes_string(x=i)) +
        geom_density(adjust = smooth, color = "blue", fill = "blue", alpha = alpha, size = 1)
    }
  } else if (type == "scatter") {

    itt <- 1
    gs <- if ("jitter" %in% check) geom_blank() else geom_point(alpha = alpha)
    for (i in xvar) {
      for (j in yvar) {
        plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + gs
        if ("log_x" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))

        if ("factor" %in% class(dat[[i]])) {
          plot_list[[itt]] <- plot_list[[itt]] +
            geom_errorbar(stat = "hline", yintercept = "mean", width = .8, size = 1, color = "blue", aes(ymax = ..y.., ymin = ..y..))
            # geom_errorbar(stat = "hline", yintercept = "median", width = .8, size = 1, color = "red", aes(ymax = ..y.., ymin = ..y..))
        }

        itt <- itt + 1
      }
    }
  } else if (type == "line") {
    itt <- 1
    for (i in xvar) {
      for (j in yvar) {
        if (color == 'none') {
          if (is.factor(dat[,i]))
            plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + geom_line(aes(group = 1))
          else
            plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + geom_line()
        } else {
          plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j, color = color)) + geom_line()
        }
        if ("log_x" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))
        itt <- itt + 1
      }
    }
  } else if (type == "bar") {
    itt <- 1
    for (i in xvar) {
      for (j in yvar) {
        plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) +
          geom_bar(stat="identity", alpha = alpha)
        itt <- itt + 1
      }
    }
  } else if (type == "box") {
    itt <- 1
    for (i in xvar) {
      dat[,i] %<>% as.factor
      for (j in yvar) {
        plot_list[[itt]] <- ggplot(dat, aes_string(x=i, y=j, fill=i)) +
                          geom_boxplot(alpha = alpha) +
                          theme(legend.position = "none")
        itt <- itt + 1
      }
    }
  }

  facets <- paste(facet_row, '~', facet_col)
  if (facets != '. ~ .') {
    scales <- if ("scale_y" %in% axes) "free_y" else "fixed"
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + facet_grid(facets, scales = scales)
  }

  if (color != 'none')
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + aes_string(color=color) + scale_fill_brewer()

  if ("jitter" %in% check)
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[1]] +
      geom_jitter(alpha = alpha)

  if ("line" %in% check)
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + geom_smooth(method = "lm", fill = 'blue',
                                             alpha = .1, size = .75,
                                             linetype = "dashed",
                                             colour = 'black')

  if ("loess" %in% check)
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + geom_smooth(span = smooth, size = .75,
                                             linetype = "dotdash", aes(group=1))

  if ("flip" %in% axes)
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + coord_flip()

  if ("log_y" %in% axes)
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + scale_y_continuous(trans = "log")

  if ("log_x" %in% axes)
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + scale_x_continuous(trans = "log")

 if (custom)
   if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

 sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = min(length(plot_list), 2)))) ) %>%
   { if (shiny) . else print(.) }
}


# library(radiant)
#
