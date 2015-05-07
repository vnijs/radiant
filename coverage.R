# use https://github.com/jimhester/covr
if (!require(covr)) {
 	devtools::install_github("jimhester/covr")
 	library(covr)
}

setwd("~/gh/radiant_dev")
library(radiant)

# define so that examples are not run
# coverage_test <<- TRUE

# cov <- package_coverage()
# test_examples()
cov <- package_coverage(type="example")
# cov <- package_coverage(type="all")

# print(cov)  # doesn't seem to be working
shine(cov)

# rm(coverage_test)
