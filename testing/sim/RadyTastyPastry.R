rm(list = ls())

########################################################
# User input
########################################################

D <- 535.74
Dsd <- 145
q <- seq(.5,1078.5,.5)

cost <- 1.25
salvage <- .5
price <- 5

nr_sim <- 1000
# prof <- -cost*q + 5*pmin(q,D) + .5 * pmax(0, q - D)

########################################################
# Running simulation with user input
########################################################
options(digits = 7)

res <- matrix(NA,nrow = length(q), ncol = nr_sim)

# could probably do this in dplyr easily
# generate one set of sims and then `gather`
# into long format
# then calculate profit per row
# calculate profits (exp, high, low) per level of q
for(i in 1:nr_sim) {
  Dsim <- rnorm(1, mean = D, sd = Dsd)
  prof <- -cost*q + 5*pmin(q,Dsim) + .5 * pmax(0, q - Dsim)
  res[,i] <- prof
  cat(i, " ")
}
cat("\n\n... and the winner is ...\n\n")

require(ggplot2)
require(dplyr)
require(tidyr)

exp_prof <- rowMeans(res)
data.frame(q = q, exp_prof = rowMeans(res)) %>%
  arrange(desc(exp_prof)) %>%
  head %>%
  print

prof <- data.frame("profits" = c(res))

# histogram of profits
ggplot(prof, aes(x=profits)) + geom_histogram(aes(y = ..density..), alpha = .3) + geom_density(adjust=1.5, color = "blue", alpha=.3) + labs(y = "") + theme(axis.text.y = element_blank())

res <- cbind(q,res)
res <- data.frame(res)
colnames(res) <- c("q",paste0("sim",1:nr_sim))

res %>%
  tbl_df %>%
  gather(sims,profit,sim1:sim1000,-q) %>%
  arrange(q) %>%
  group_by(q) %>%
  summarise(exp_prof = mean(profit), low_prof = quantile(profit,.025), high_prof = quantile(profit, .975)) %>%
  arrange(q) -> fin_res

# profit curve
ggplot(fin_res, aes(x = q, y = exp_prof)) +
  geom_line() +
  scale_y_continuous(name="Expected profits") +
  scale_x_continuous(name="Quantity ordered")

ggplot(fin_res, aes(x = q, y = exp_prof)) +
  geom_line() +
  geom_ribbon(data=fin_res,aes(ymin=low_prof,ymax=high_prof),alpha=0.3) +
  scale_y_continuous(name="Expected profits") +
  scale_x_continuous(name="Quantity ordered")
