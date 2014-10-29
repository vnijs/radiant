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
