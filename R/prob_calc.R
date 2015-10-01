#' Probability calculator
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/probability.html} for an example in Radiant
#'
#' @param dist Distribution
#' @param mean Mean
#' @param stdev Standard deviation
#' @param lb Lower bound
#' @param ub Upper bound
#'
#' @return Probability stuff
#'
#' @export

# pnorm(600, 500, 100)
# p <- 1 - pnorm(600, 500, 100)
# qnorm(p, 500, 100, lower.tail = FALSE)
# lb <- mean - 4*sd
# ub <- mean + 4*sd
# lb <- 8
# ub <- 11
# min <- -Inf
# max <- Inf

prob_calc <- function(dist = "normal", mean = 0, stdev = 1, lb = -Inf, ub = Inf) {

  limits <- c(mean - 3*stdev, mean + 3*stdev)
  dnorm_limit <- function(x) {
    y <- dnorm(x, mean = mean, sd = stdev)
    y[x < lb | x > ub] <- NA
    y
  }

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	ggplot(data.frame(x=limits), aes(x=x)) +
	  stat_function(fun=dnorm, args = list(mean = mean, sd = stdev)) +
	  stat_function(fun=dnorm_limit, geom="area", fill="blue", alpha=0.2) +
	  xlab("") + ylab("")
}

# plot_prob_calc(mean = 500, stdev = 100, ub = 600)
# plot_prob_calc(mean = 500, stdev = 100, lb = 600)
# plot_prob_calc(mean = 0, stdev = 1, ub = 1)
# plot_prob_calc(mean = 20, stdev = 1, ub = 20)

plot_prob_calc <- function(dist = "normal", mean = 0, stdev = 1, lb = -Inf, ub = Inf) {

  limits <- c(mean - 3*stdev, mean + 3*stdev)
  dnorm_limit <- function(x) {
    y <- dnorm(x, mean = mean, sd = stdev)
    y[x < lb | x > ub] <- NA
    y
  }

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	ggplot(data.frame(x=limits), aes(x=x)) +
	  stat_function(fun=dnorm, args = list(mean = mean, sd = stdev)) +
	  stat_function(fun=dnorm_limit, geom="area", fill="blue", alpha=0.2) +
	  xlab("") + ylab("")
}

# output$mc3_density_interactive <- renderUI({
#   tagList(
#     sliderInput("mc3_area", label = "Spending area:",
#       min = 0, max = 16, value = state_init("mc3_area",c(0,16)), step = .5),
#     renderPlot({
#       if(is_empty(input$mc3_area)) return()
#       density_plot(lb = input$mc3_area[1], ub = input$mc3_area[2],
#                    dat = mc3_dat$spending)
#     })
#   )
# })
