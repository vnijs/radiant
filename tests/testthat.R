# use shift-cmd-t to run all tests
library(testthat)
library(radiant)

test_check("radiant")

# if (interactive() && !exists("coverage_test")) devtools::run_examples()
# if (interactive()) devtools::run_examples(start = "regression")
# warnings()
