setwd("~/gh/radiant_gh-pages/")
# setwd('quant'); system('make')

# local_dir <- .libPaths()[1]
# global_dir <- .libPaths()[2]

# installing packages to global_dir
# repos <- "http://cran.rstudio.com"
# options(repos = c(CRAN = repos))
# install.packages("rmarkdown", lib = global_dir)
# install.packages("ggvis", lib = global_dir)
# install.packages("testthat", lib = global_dir)
# devtools::install_github

file.copy("../radiant_dev/inst/marketing/data/shopping.rda",
          "~/Desktop/shopping.rda",overwrite = TRUE)

system('make')
setwd('base/app')
knitr::knit('combine.Rmd'); knitr::knit('view.Rmd'); knitr::knit('transform.Rmd');
setwd('../../base'); system('make')
setwd('../marketing'); system('make')
setwd('../quant'); system('make')
setwd('../')

# making the README.md file after clean-up

library(knitr)
knitr::knit("README.Rmd")
setwd("sub")
knitr::knit("README_dev.Rmd")
file.copy("README_dev.md","../../radiant_dev/README.md",overwrite = TRUE)
file.copy("README_dev.md","../../radiant_dev/inst/base/tools/app/about.md",overwrite = TRUE)
knitr::knit("tutorials_dev.Rmd")
file.copy("tutorials_dev.md","../../radiant_dev/inst/base/tools/app/tutorials.md",overwrite = TRUE)
setwd('../')

unlink("~/Desktop/shopping.rda")
system('sh rsync_base2app.sh')

# create documentation pdf
unlink('radiant.pdf')
# create pdf of documentation
setwd("~")
unlink('radiant.pdf')
system("R CMD Rd2pdf radiant --no-preview")
system("rm -rf .Rd2pdf*")
setwd("~/gh/radiant_gh-pages/")
file.copy("~/radiant.pdf","radiant.pdf",overwrite = TRUE)
system("rm -rf .Rd2pdf*")

