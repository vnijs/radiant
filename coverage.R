# use https://github.com/jimhester/covr
if (!require(covr)) {
 	devtools::install_github("jimhester/covr")
 	library(covr)
}

setwd("~/gh/radiant")

# cov <- package_coverage(type = "example")
cov <- package_coverage(type = "all")
shine(cov)

# after_success:
#   - Rscript -e 'library(covr); coveralls()'
