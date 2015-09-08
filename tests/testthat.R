## use shift-cmd-t to run all tests
# setwd("~/gh/radiant")
library(testthat)
#library(radiant)

test_check("radiant")

# if (interactive() && !exists("coverage_test")) devtools::run_examples()
# if (interactive()) devtools::run_examples(start = "regression")

# setwd("~/gh/radiant")
# devtools::run_examples()
# warnings()

## https://github.com/hadley/testthat/issues/255
## See tests/Examples/create-examples.sh

## https://github.com/hadley/testthat/issues/255
## Next step: Explore https://github.com/wch/vtest to test plots
