# source("~/gh/radiant_dev/R/radiant.R")
# pkgs_cran <- c("car", "gridExtra", "GPArotation", "psych", "wordcloud",
#                "AlgDesign", "knitr", "lubridate", "ggplot2", "ggdendro",
#                "shiny", "pryr", "shiny", "magrittr", "tidyr", "dplyr",
#                "broom", "htmlwidgets")
# pkgs_gh <- c("shinyAce","rpivotTable")
# pkgs <- c(pkgs_cran, pkgs_gh)
# sapply(pkgs, require, character.only=TRUE)

# visualize("diamonds", "carat", "price", viz_type = "scatter", viz_check = "loess")
# visualize("diamonds", "carat:x", viz_yvar = "price", viz_type = "scatter")
# visualize("diamonds", "price:x", viz_type = "hist")


# source("~/gh/radiant_dev/R/visualize.R")

# visualize("diamonds", "price", viz_type = "hist")

# dat <- getdata("diamonds")
# bw <- select_(dat,"price") %>% range %>% diff(.)/15
# p <- ggplot(dat, aes_string(x="price"))
# p <- p + geom_histogram(aes(y = ..density..),  binwidth = bw)
# p

# + stat_ecdf()
# ef <- ecdf(dat$price) %>% data.frame
# plot(ef)
# ef
# class(ef)
# dim(dat)
# p

# p

# df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)), g = gl(2, 100))
# ggplot(df, aes(x, colour = g)) + geom_histogram() + stat_ecdf()

# p = ggplot(data = dat, aes(x = drivenkm)) +
#    geom_histogram(aes(y = 3*..density..), alpha = 0.2, binwidth = 3) +
#    stat_ecdf(aes(x = drivenkm))

# # p = ggplot(data = tmp, aes(x = drivenkm)) +
# #    geom_histogram(aes(y = 3*..density..), alpha = 0.2, binwidth = 3) +
# #    stat_ecdf(aes(x = drivenkm))
