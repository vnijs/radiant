require(magrittr)
# packageVersion('magrittr')
require(dplyr)

rnorm(200) %>%
  matrix(ncol = 2) %T>% print %>%
  colSums

help("%T>%")

trig_fest <- . %>% tan %>% cos %>% sin

trig_fest
trig_fest(1:10)
vignette("magrittr")

rnorm(1000)    %>%
  multiply_by(5) %>%
  add(5)         %>%
{
  cat("Mean:", mean(.),
      "Variance:", var(.), "\n")
  head(.)
}

(rnorm(1000) * 5 + 5)    %>%
{
  cat("Mean:", mean(.),
      "Variance:", var(.), "\n")
  head(.)
}

?multiply_by

library("ggplot2")
library("magrittr")
library("dplyr")

echo <- function(x) {
  print(x)
  x
}

mtcars %T>%
  qplot(mpg, cyl, data = .) %T>%
  summarise(mean(mpg))

library("ggplot2")
library("magrittr")
library("dplyr")

echo <- function(x,gg) {
  print(gg)
  x
}
mtcars %>%
  echo(., qplot(mpg, cyl, data = .)) %>%
  summarise(mean(mpg))

mtcars %T>%
  qplot(mpg, cyl, data=.) %>%
  summarise(mean_mpg = mean(mpg))
