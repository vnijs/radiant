library(radiant)

# analysis on built-in dataset mtcars
result <- single_mean("mtcars","mpg",sm_comp_value = 20, sm_alternative = "greater")
summary_single_mean(result)
plots_single_mean(result)

result <- compare_means("mtcars","cyl","mpg")
summary_compare_means(result)
plots_compare_means(result)

# MAYBE USE SWITCH CREATE A LINK BETWEEN
# rsummary(result)
# rplot(result)

# in radiant load Examples > Load examples from the Data > Manage tab
radiant()


# a <- new.env()
# a$test <- list()
# a$test[[1]] <- rnorm(100)
# a$test[[1]]
# rm(a)


# library(pryr)
# environmentName(environment())
# env.profile(environment())
# r_env
# object_size(r_env)
# r_env_copy <- r_env
# object_size(r_env_copy)
# object_size(r_env_copy, r_env)
# r_env$r_data
# rm(r_env_copy)
# r_env

result <- compare_means("salary","rank","salary")
summary_compare_means(result)
plots_compare_means(result)

# should produce an error
result <- try(compare_means("no_dat","cyl","mpg"), silent = TRUE)
if(is(result, 'try-error')) print("Error produced as expected")
