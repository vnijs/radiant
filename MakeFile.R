setwd("~/gh/radiant_gh-pages/")

# install.packages('rmarkdown')
# library(rmarkdown)

file.copy("../radiant_dev/inst/marketing/data/data_rady/shopping.rda",
          "~/Desktop/shopping.rda",overwrite = TRUE)

system('make')
setwd('base'); system('make')
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

system('rsync_base2app.sh')

# sprintf("Version: %s, Date: %s", packageVersion("radiant"), packageDescription("radiant", fields = "Date"))
