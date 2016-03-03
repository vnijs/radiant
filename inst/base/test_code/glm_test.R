library(broom)
dat1 <-
  data.frame(
    y = as.factor(c("yes","no","yes","no")),
    wts = c(50, 100, 10, 140),
    x = c("a","a","b","b")
  )
m1 <- glm(y ~ x, weights = wts, data = dat1, family = "binomial")
summary(m1)
glance(m1)

dat2 <-
  data.frame(
    y = as.factor(c(rep("yes",50), rep("no",100), rep("yes",10), rep("no",140))),
    x = rep(c("a","b"), each = 150)
  )
m2 <- glm(y ~ x, data = dat2, family = "binomial")
summary(m2)
glance(m2)
