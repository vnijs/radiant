library(ggplot2)

# http://stackoverflow.com/questions/7961865/creating-confidence-area-for-normally-distributed-scatterplot-in-ggplot2-and-r

# get data
dat <- read.csv('~/Desktop/heights.csv')

# estimate model
mod <- lm(dheight ~ mheight, data = dat)

# add prediction intervals
mod.pred = data.frame(dat, predict(mod, interval = 'prediction'))

# plot regression with  confidence interval
p <- ggplot(mod.pred, aes(x = mheight, y = dheight)) +
      geom_point() +
      geom_smooth(method = 'lm', aes(fill = 'confidence'), alpha = 0.1, fill = 'blue', size = .75, linetype = "dashed", colour = "black")
print(p)

# plot regression line with prediction interval
p <- p +  geom_ribbon(aes(y = fit, ymin = lwr, ymax = upr, fill = 'prediction'), alpha = 0.1) +
  scale_fill_manual('Interval', values = c('green', 'blue')) +
  theme(legend.position="none")
print(p)



N = 30; B0 = runif(1, -5, 5); B1 = runif(1, -5, 5)
df = local({
  x = seq(-3, 3, length = N)
  data.frame(x = x, y = B0 + B1*x + rnorm(N))
})

input <- list()
input$b0 <- 1
input$b1 <- 2
input$showFit <- TRUE

    b0 = input$b0; b1 = input$b1
#     par(mar = c(4, 4.3, .1, .1))
par(NULL)

    plot(y ~ x, data = df, col = 'darkgray', pch = 19, axes = F)
    axis(1, at = -2:2 )
    axis(2, at = -2:2 )
?axis
    axis(2)
    abline(v = 0, h = 0)
    axes(1, )




plot(c(-out$lim,out$lim),type = "n",xlab='', ylab='', axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-out$lim, out$lim), xlim=c(-out$lim,out$lim))
title(paste("Dimension",i,"vs Dimension",j), cex.main = out$mds_fontsz)
points(out$points[,i], out$points[,j], pch = 16, cex = .6)


    abline(b0, b1, col = 'red')
    fit = lm(y ~ x, data = df)
    if (input$showFit) abline(fit)
    usr = par('usr'); y0 = usr[3]; y1 = usr[4]; x0 = usr[1]; x1 = usr[2]
    x2 = x1 - .05 * (x1 - x0)
    rect(x2, y0, x1, y1, border = 'darkgray')
    with(df, {
      res = y - (b0 + b1 * x)  # residuals (not really OLS residuals)
      sst = sum((y - mean(y))^2)
      sse = sum(res^2)
      rect(x2, y0, x1, y0 + sse/sst * (y1 - y0), col = rgb(1, 0, 0, .5), border = NA)
      text(x2, (y0 + y1)/2, sprintf('SSE = %.02f', sse), pos = 2)
      if (input$showResid) {
        segments(x, y0, x, y0 + abs(res), col = 'red', lty = ifelse(res > 0, 1, 2))
      }
    })
  }, width = 700, height = 500)
})


library(ggplot2)
# install.packages('ggvis')
library(ggvis)
dia %>% ggvis(prop("x", as.name("carat")), prop("y", as.name("price")))



a <- c(2,3,4)
a <- as.factor(a)
a
a <- as.character(a)
a
a <- as.integer(a)
a



