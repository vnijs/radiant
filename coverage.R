# use https://github.com/jimhester/covr
if (!require(covr)) {
 	devtools::install_github("jimhester/covr")
 	library(covr)
}
setwd("~/gh/radiant_dev")

# define so that examples are not run
coverage_test <<- TRUE

cov <- package_coverage()
# print(cov)  # doesn't seem to be working
shine(cov)

rm(coverage_test)

# add to travis.yml when coverage gets a little higher :)
# r_github_packages:
#   - jimhester/covr

# after_success:
#   - Rscript -e 'library(covr);coveralls()'
