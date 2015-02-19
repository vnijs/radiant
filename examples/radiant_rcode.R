library(radiant)

# analysis on built-in dataset mtcars
result <- single_mean("mtcars", "mpg", sm_comp_value = 20, sm_alternative = "greater")
summary(result)
plot(result)

result <- compare_means("mtcars", "cyl", "mpg")
summary(result)
plot(result)

result <- compare_means("diamonds", "cut", "price")
summary(result)
plot(result)

# in radiant load Examples > Load examples from the Data > Manage tab
radiant()

# this result will differ from the previous one because the data from radiant is used which includes only a subset of all diamond prices for illustration purposes
result <- compare_means("diamonds", "cut", "price")
summary(result)
plot(result)

# if the salary data was loaded in radiant you should get output here
result <- compare_means("salary", "rank", "salary")
summary(result)
plot(result)

# should produce an error because no dataset with that name exists
result <- try(compare_means("no_dat", "cyl", "mpg"), silent = TRUE)
if(is(result, 'try-error')) print("Error produced as expected")

# regression
result <- regression("diamonds", "price", c("carat","clarity"), reg_plot = c("coef"))
summary(result)
plot(result)
result <- regression("diamonds", "price", c("carat","clarity"), reg_plot = c("dashboard"))
plot(result)
?regression






