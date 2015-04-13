# use https://github.com/jimhester/covr
if (interactive()) {

	# setwd("~/gh/radiant_dev/")
	if(!require(covr)) {
	 	devtools::install_github("jimhester/covr")
	 	library(covr)
	}

	# cov <- package_coverage(path = file.path(system.file(package = "radiant"),"tests"))
	# cov <- package_coverage(path = normalizePath(file.path(t_path,"../")))
	# cov <- package_coverage(path = "~/gh/radiant_dev/tests")
	# cov <- package_coverage(path = "./tests")
	cov <- package_coverage()
	print(cov)
	shine(cov)

}

# add to travis.yml when coverage gets a little higher :)
# r_github_packages:
#   - jimhester/covr

# after_success:
#   - Rscript -e 'library(covr);coveralls()'
