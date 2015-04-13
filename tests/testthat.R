library(testthat)

curr_path <- getwd()
t_path <- "~/gh/radiant_dev/tests"
setwd(t_path)

test_check("radiant")

if (interactive()) devtools::run_examples()
# if (interactive()) devtools::run_examples(start = "radiant")
# if (interactive()) devtools::run_examples(start = "regression")
# warnings()

setwd(curr_path)
rm(t_path, curr_path)
