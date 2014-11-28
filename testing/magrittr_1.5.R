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
