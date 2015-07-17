shinyUI(
  do.call(navbarPage, c("Quantitative Analysis", nav_ui, quant_ui, shared_ui))
)

# library(lubridate)
# Date <- as.Date("2013-12-31") + 1:365
# Month <- month(Date, label = TRUE, abbr = FALSE) %>% factor(ordered = FALSE)
# Trend <- 1:365
# Demand <- runif(365, 0, 1)
#
# dat <- data.frame(Date, Month, Trend, Demand)
#
#
#
# dat %>% summarise_each(funs(n_distinct))
# dat %>% summarise_each(funs(is.factor,n_distinct))
#
# summarise_each(funs(n_distinct))
# )
#
# varnames <- function() colnames(dat)
#
# # f <- function(x) is.factor | (n_distinct > 11 & n_distinct > 1)
# f <- function(x) n_distinct > 11
# && n_distinct > 1
#
# dat %>% summarise_each(funs(is.factor(.) | length(unique(.)) %in% 2:20))
#
# %>%
#   ?summarise_each
#   {. < 13 & . > 1} %>%
#   which(.) %>%
#   varnames()[.]
#
# dat %>% summarise_each(funs(is.factor)) %>% {. == TRUE} %>% which
#
#   .[1,] %>% as.vector %>% class
#
# %>% which
#
# [1,]
#
# %>%
#   which(.) %>%
#   varnames()[.]
#
#   iris %>% summarise_each(funs(length(unique(.))))
#
