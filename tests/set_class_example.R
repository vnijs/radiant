# setwd('quant'); system('make'); setwd('../')
# setwd('marketing'); system('make'); setwd('../')
options(repos = c(XRAN = "http://mostly-harmless.github.io/radiant_miniCRAN/"))
install.packages("radiant")
library('radiant')


radiant()

## Define a print method that will be automatically dispatched when print()
## is called on an object of class "myMatrix"
print.myMatrix <- function(x) {
  n <- nrow(x)
  for(i in seq_len(n)) {
    cat(paste("This is row", i, "\t: " ))
    cat(x[i,], "\n")
  }
}

## Make a couple of example matrices
m <- mm <- matrix(1:16, ncol=4)

class(m)

set_class <- `class<-`

m %>% set_class(c('myMatrix',class(.)))
class(m)


## Create an object of class "myMatrix".
class(m) <- c("myMatrix", class(m))
## When typed at the command-line, the 'print' part of the read-eval-print loop
## will look at the object's class, and say "hey, I've got a method for you!"
m
class(m)
# This is row 1   : 1 5 9 13
# This is row 2   : 2 6 10 14
# This is row 3   : 3 7 11 15
# This is row 4   : 4 8 12 16

## Alternatively, you can specify the print method yourself.
print.myMatrix(mm)
print(m)


library(radiant)

result <- single_mean("mtcars", "mpg")
class(result) <- c("single_mean", class(result))
summary.single_mean <- summary_single_mean
plot.single_mean <- plots_single_mean
summary(result)
plot(result)



library(magrittr)
?multiply_by

set_class <-

  set_names
.Primitive("class<-")
class
