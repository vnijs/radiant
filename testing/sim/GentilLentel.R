rm(list = ls())

# Economics  			Probability
# Non-Labor Costs	3995	/month		Constant
# Labor Costs	5040	/month		Uniform
# 6860	/month
# Meal Revenue	20	/meal		0.25
# 18.5	/meal		0.35
# 16.5	/meal		0.3
# 15	/meal		0.1
# Number Meals	3000	/month	(mean)	Normal
# 1000	/month	(stdev)
# Unit Meal Cost	11	/meal		Constant

########################################################
# User input
########################################################

non_labor_cost <- 3995          # constant
labor_cost <- c(5040,6860)      # uniform

price <- c(20,18.5,16.5,15)     # discrete values
price_prob <- c(.25,.35,.3,.1)  # discrete probs

nr_meals <- c(3000,1000)        # normal

cost <- 11                      # constant
nr_sim <- 1000                  # constant

Es <- (price - cost)*nr_meals - cost - non_labor_cost

########################################################
# Running simulation with user input
########################################################
options(digits = 7)

# res <- matrix(NA,nrow = length(q), ncol = nr_sim)

labor_cost_sim <- runif(10, labor_cost[1], labor_cost[2])
nr_meals_sim <- rnorm(nr_sim,nr_meals[1],nr_meals[2])

price_sim <- sample(price, nr_sim, replace = TRUE, prob = price_prob)
# table(price_sim)

Es <- (price_sim - cost)*nr_meals_sim - labor_cost_sim - non_labor_cost

mean(Es)
sd(Es)

sum(Es < 5000) / length(Es)
sum(Es > 6667) / length(Es)


# could probably do this in dplyr easily
# generate one set of sims and then `gather`
# into long format
# then calculate profit per row
# calculate profits (exp, high, low) per level of q

require(ggplot2)
# require(dplyr)
# require(tidyr)

Es <- data.frame(Es = Es)
bw <- diff(range(Es, na.rm = TRUE)) / 20

# histogram of profits
ggplot(Es, aes(x=Es)) + geom_histogram(aes(y = ..density..), binwidth = bw, alpha = .3) + geom_density(adjust=1.5, color = "blue", alpha=.3) + labs(y = "") + theme(axis.text.y = element_blank())


# Annual

nr_meals_sim <- rnorm(nr_sim*12,nr_meals[1],nr_meals[2])
nr_meals_sim <- matrix(nr_meals_sim, nrow = nr_sim, ncol = 12)

Es <- (price_sim - cost)*nr_meals_sim - labor_cost_sim - non_labor_cost
dim(Es)

Es_annual <- rowSums(Es)

mean(Es_annual)
sd(Es_annual)

Es_annual <- data.frame(Es_annual = Es_annual)
bw <- diff(range(Es_annual, na.rm = TRUE)) / 20

# histogram of profits
ggplot(Es_annual, aes(x=Es_annual)) + geom_histogram(aes(y = ..density..), binwidth = bw, alpha = .3) + geom_density(adjust=1.5, color = "blue", alpha=.3) + labs(y = "") + theme(axis.text.y = element_blank())
