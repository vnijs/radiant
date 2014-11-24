dat <- mtcars

names(dat)
rename(dat,)

rename(dat, mympg = mpg, mycyl = cyl)

rename_(dat, .dots = c("mympg" = "mpg", "mycyl" = "cyl"))
rename_(dat, .dots = tt)

dat
rename_(dat, .dots = setNames(l2,l1))

ml <- list()
ml[[c("mympg", "mycyl")]]

l1 <- c("mympg", "mycyl")
l2 <- c("mpg", "cyl")
tt <- setNames(l2,l1)

names(dat)[1:length(l1)] <- l1

changes(dat,mtcars)

dat <- mtcars
dat <- rename_(dat, .dots = setNames(l2,l1))

changes(dat,mtcars)

