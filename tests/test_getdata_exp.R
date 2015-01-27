library(radiant)

# should produce an error
result <- try(compare_means("x","cyl","mpg"), silent = TRUE)
if(is(result, 'try-error')) print("Error produced as expected")

# analysis on built-in dataset mtcars
result <- compare_means("mtcars","cyl","mpg")
summary_compare_means(result)
plots_compare_means(result)

# in radiant load Examples > Load examples from the Data > Manage tab
radiant()
result <- compare_means("salary","rank","salary")
summary_compare_means(result)
plots_compare_means(result)
