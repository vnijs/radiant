library(knitr)
library(dplyr)

## generate (updated) html and md files
setwd("~/gh/docs/")
system('make')

knit_docs <- function(x) {
	setwd(file.path("~/gh/docs",x,"app"))
	list.files(pattern = "*.Rmd") %>% sapply(knit)
	setwd(file.path("../..",x)); system("make")
}

sapply(c("data","design","basics","model","multivariate"), knit_docs)

## making the README.md file after clean-up
# setwd("~/gh/docs/")
# knit("README.Rmd")
# setwd("~/gh/docs/sub")
# knit("README_dev.Rmd")
# knit("tutorials_dev.Rmd")
#
# file.copy("README_dev.md",file.path("../../radiant.data/inst/app/tools/app/about.md"),overwrite = TRUE)
# file.copy("tutorials_dev.md",file.path("../../radiant.data/inst/app/tools/app/tutorials.md"),overwrite = TRUE)
#
# copy_docs <- function(app) file.copy("README_dev.md",file.path("../..",app,"README.md"),overwrite = TRUE)
# sapply(c("radiant","radiant.data","radiant.design","radiant.basics","radiant.model","radiant.multivariate"), copy_docs)

## sync (R)md files to gh/radiant
# setwd("~/gh/docs")
# system('sh rsync_docs2app.sh')
#
# ## create documentation pdfs
# unlink('radiant.data.pdf')
# setwd("~")
# unlink('radiant.data.pdf')
# system("R CMD Rd2pdf gh/radiant.data --no-preview")
# system("rm -rf .Rd2pdf*")
# setwd("~/gh/docs/")
# file.copy("~/radiant.data.pdf","radiant.data.pdf",overwrite = TRUE)
# system("rm -rf .Rd2pdf*")
#
# create_manuals <- function(x) {
# 	app <- paste0("radiant.",x)
# 	man <- paste0(app,".pdf")
# 	unlink(man)
# 	setwd("~")
# 	unlink(man)
# 	system(paste0("R CMD Rd2pdf gh/",app," --no-preview"))
# 	system("rm -rf .Rd2pdf*")
# 	setwd("~/gh/docs/")
# 	file.copy(paste0("~/",man), man, overwrite = TRUE)
# 	system("rm -rf .Rd2pdf*")
# }
#
# sapply(c("data","design","basics","model","multivariate"), create_manuals)
