library(knitr)
library(dplyr)
setwd("~/gh/radiant_gh-pages/")

## generate (updated) html and md files
system('make')
setwd('base/app')
list.files(pattern = "*.Rmd") %>% sapply(knit)
setwd('../../base'); system('make')
setwd('../marketing'); system('make')
setwd('../analytics'); system('make')
setwd('../quant/app');
list.files(pattern = "*.Rmd") %>% sapply(knit)
setwd('../../quant'); system('make')
setwd('../')

## making the README.md file after clean-up
knit("README.Rmd")
setwd("sub")
knit("README_dev.Rmd")
file.copy("README_dev.md","../../radiant/README.md",overwrite = TRUE)
file.copy("README_dev.md","../../radiant/inst/base/tools/app/about.md",overwrite = TRUE)
knit("tutorials_dev.Rmd")
file.copy("tutorials_dev.md","../../radiant/inst/base/tools/app/tutorials.md",overwrite = TRUE)
setwd('../')

## sync (R)md files to gh/radiant
system('sh rsync_base2app.sh')

## create documentation pdf
unlink('radiant.pdf')
setwd("~")
unlink('radiant.pdf')
system("R CMD Rd2pdf gh/radiant --no-preview")
system("rm -rf .Rd2pdf*")
setwd("~/gh/radiant_gh-pages/")
file.copy("~/radiant.pdf","radiant.pdf",overwrite = TRUE)
system("rm -rf .Rd2pdf*")
