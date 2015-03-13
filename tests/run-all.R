library(radiant)
test_dir <- "~/gh/radiant_dev/tests/"
trim_trailing <- function (x) sub("\\s+$", "", x)
trim_leading <- function (x) sub("^\\s+", "", x)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

compare_output <- function(res1, res2) {
  for(i in 1:length(res2)) {
    if(res1[i] != res2[i]) {
      print(i)
      print(res1[i])
      print(res2[i])
    }
  }
}

######################################################
# regression
######################################################
result <- regression("diamonds", "price", c("carat", "clarity"))
res1 <- capture.output(summary(result)) %>% trim
# cat(paste0(res1,"\n"), file = "~/gh/radiant_dev/tests/output/regression1.txt")
res2 <- paste0(readLines(paste0(test_dir,"output/regression1.txt"))) %>% trim
all.equal(res1,res2)
# compare_output(res1,res2)

# png(paste0(test_dir,"/output/regression1.png"))
#   plot(result, reg_plots = "dashboard")
# dev.off()

# res1 <- readPNG(paste0(test_dir,"/output/regression1.png"))
# res2 <- readPNG(paste0(test_dir,"/output/regression1c.png"))
# all.equal(res1,res2)

######################################################
# single_prop
######################################################
result <- single_prop("diamonds","cut")
res1 <- capture.output(summary(result)) %>% trim
# cat(paste0(res1, "\n"), file = "~/gh/radiant_dev/tests/output/single_prop1.txt")
res2 <- paste0(readLines(paste0(test_dir,"output/single_prop1.txt"))) %>% trim
all.equal(res1,res2)
# compare_output(res1,res2)

result <- single_prop("diamonds","clarity", sp_levels = "IF", sp_comp_value = 0.05)
res1 <- capture.output(summary(result)) %>% trim
# cat(paste0(res1, "\n"), file = "~/gh/radiant_dev/tests/output/single_prop2.txt")
res2 <- paste0(readLines(paste0(test_dir,"output/single_prop2.txt"))) %>% trim
all.equal(res1,res2)
# compare_output(res1,res2)







