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
#' @param bins Number of bins used for a histogram (not accessible in Radiant)
#' @param smooth Adjust the flexibility of the loess line for scatter plots (not accessible in Radiant)
#' @param check Add a regression line ("line"), a loess line ("loess"), or jitter ("jitter") to a scatter plot
#' @param axes Flip the axes in a plot ("flip") or apply a log transformation (base e) to the y-axis ("log_y") or the x-axis ("log_x")
#' @param shiny Did the function call originate inside a shiny app
#' @param data_filter Expression used to filter the dataset. This should be a string (e.g., "price > 10000")
#'
#' @return Generated plots
#'
#' @examples
#' visualize("diamonds", "carat", "price", type = "scatter", check = "loess")
#' visualize("diamonds", "price:x", type = "hist")
#' visualize("diamonds", "carat:x", yvar = "price", type = "scatter")
#' diamonds %>% visualize(c("price","carat","depth"), type = "density")
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
                      data_filter = "",
                      shiny = FALSE) {

  ## inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
  vars <- xvar

  if (!type %in% c("scatter","line"))
    color = "none"

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

  plots <- list()

  if (type == "hist") {
    for (i in xvar) {
      plots[[i]] <- ggplot(dat, aes_string(x=i))
      if ("factor" %in% class(dat[,i])) {
        plots[[i]] <- plots[[i]] + geom_histogram()
      } else {
        bw <- select_(dat,i) %>% range %>% diff(.)/bins
        plots[[i]] <- plots[[i]] + geom_histogram(binwidth = bw)
      }
    }
  } else if (type == "density") {
    for (i in xvar) {
      plots[[i]] <- ggplot(dat, aes_string(x=i)) +
                    geom_density(adjust=smooth, fill = "green", alpha=.3)
    }
  } else if (type == "scatter") {
    itt <- 1
    gs <- if ("jitter" %in% check) geom_blank() else geom_point(alpha = .5)
    for (i in xvar) {
      for (j in yvar) {
        plots[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + gs
        if ("log_x" %in% axes) plots[[itt]] <- plots[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plots[[itt]] <- plots[[itt]] + ylab(paste("log", j))
        itt <- itt + 1
      }
    }
  } else if (type == "line") {
    itt <- 1
    for (i in xvar) {
      for (j in yvar) {
        if (color == 'none') {
          if (is.factor(dat[,i]))
            plots[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + geom_line(aes(group = 1))
          else
            plots[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + geom_line()
        } else {
          plots[[itt]] <- ggplot(dat, aes_string(x=i, y=j, color = color)) + geom_line()
        }
        if ("log_x" %in% axes) plots[[itt]] <- plots[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plots[[itt]] <- plots[[itt]] + ylab(paste("log", j))
        itt <- itt + 1
      }
    }
  } else if (type == "bar") {
    itt <- 1
    for (i in xvar) {
      for (j in yvar) {
        plots[[itt]] <- ggplot(dat, aes_string(x=i, y=j)) + geom_bar(stat="identity")
        itt <- itt + 1
      }
    }
  } else if (type == "box") {
    itt <- 1
    for (i in xvar) {
      dat[,i] %<>% as.factor
      for (j in yvar) {
        plots[[itt]] <- ggplot(dat, aes_string(x=i, y=j, fill=i)) +
                          geom_boxplot(alpha = .7) +
                          theme(legend.position = "none")
        itt <- itt + 1
      }
    }
  }

  facets <- paste(facet_row, '~', facet_col)
  if (facets != '. ~ .') {
    scales <- if ("scale_y" %in% axes) "free_y" else "fixed"
    for (i in 1:length(plots))
      plots[[i]] <- plots[[i]] + facet_grid(facets, scales = scales)
  }

  if (color != 'none')
    for (i in 1:length(plots))
      plots[[i]] <- plots[[i]] + aes_string(color=color) + scale_fill_brewer()

  if ("jitter" %in% check)
    for (i in 1:length(plots)) plots[[i]] <- plots[[1]] + geom_jitter(alpha = .5)

  if ("line" %in% check)
    for (i in 1:length(plots))
      plots[[i]] <- plots[[i]] + geom_smooth(method = "lm", fill = 'blue',
                                             alpha = .1, size = .75,
                                             linetype = "dashed",
                                             colour = 'black')

  if ("loess" %in% check)
    for (i in 1:length(plots))
      plots[[i]] <- plots[[i]] + geom_smooth(span = smooth, size = .75,
                                             linetype = "dotdash", aes(group=1))

  if ("flip" %in% axes)
    for (i in 1:length(plots)) plots[[i]] <- plots[[i]] + coord_flip()

  if ("log_y" %in% axes)
    for (i in 1:length(plots))
      plots[[i]] <- plots[[i]] + scale_y_continuous(trans = "log")

  if ("log_x" %in% axes)
    for (i in 1:length(plots))
      plots[[i]] <- plots[[i]] + scale_x_continuous(trans = "log")

 sshhr( do.call(arrangeGrob, c(plots, list(ncol = min(length(plots), 2)))) ) %>%
   { if (shiny) . else print(.) }

}
