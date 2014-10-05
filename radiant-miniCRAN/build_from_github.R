# to install the update to Shiny use the below
build_pkgs <- c("shiny","shinyAce","rvest","tidyr","dplyr","ggvis")

# path to non-local install directory
inst_to <- .libPaths()[2]
options(repos = c(CRAN = "http://cran.rstudio.com"))

# building packages using devtools
install.packages('devtools')
library(devtools)
install_github('hadley/devtools')
install.packages('roxygen2')

# build('shiny')
# build('shiny', binary = TRUE)

# build for windows
# build_win(pkg = ".", version = c("R-release", "R-devel"), args = NULL, quiet = FALSE)

# library('dplyr')
# install.packages('nycflights13')
# library(nycflights13)
# dim(flights)
# filter(flights, month == 1, day == 1)
# arrange(flights, desc(arr_delay))
# mutate(flights, gain = arr_delay - dep_delay, speed = distance / air_time * 60)
#
# library(ggplot2)
# planes <- group_by(flights, tailnum)
# delay <- summarise(planes,
#                    count = n(),
#                    dist = mean(distance, na.rm = TRUE),
#                    delay = mean(arr_delay, na.rm = TRUE))
# delay <- filter(delay, count > 20, dist < 2000)
#
# # Interestingly, the average delay is only slightly related to the
# # average distance flown by a plane.
# ggplot(delay, aes(dist, delay)) +
#   geom_point(aes(size = count), alpha = 1/2) +
#   geom_smooth() +
#   scale_size_area()
