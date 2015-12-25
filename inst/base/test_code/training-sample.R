n <- .7
dataset <- mtcars

vars <- c("gear","am")
dat <- dataset %>% group_by_(.dots = vars)
name <- "training"

grps <- dataset %>% group_by_(.dots = vars) %>% attr("indices")
nr <- length(grps)
mt <- make_train(n, nr)
# mtfin <- rep(0, nrow(dat))
# mtfin <- data.frame(matrix(0, nrow(dataset))) %>% setNames(name)

dataset %>% group_by_(.dots = vars) %>% attr("indices")

r_data <- list()
r_data$dat <- mtcars
dataset <- "dat"

mutate(r_data[[dataset]], training = make_train(.7, n()))



training <- rep_len(0L,nrow(dataset))
training


for (i in 1:nr) {
  mtfin[grps[[i]]] <- mt[i]
}

mutate(r_data[[dataset]], training = make_train(.7, n()))

# group_by(r_data[[dataset]], gear, am) %>% mutate(training = make_train(.7, n(), attr(., "indices")))
group_by(r_data[[dataset]], gear, am) %>% {make_train(.7, nrow(.), attr(., "indices"))}

group_by(r_data[[dataset]], gear, am) %>% sample_frac(.30, 1)
?sample_frac

# mutate(r_data[[dataset]], training = make_train(.7, n(), {attr(group_by(., gear, am), "indices")}))
mutate(r_data[[dataset]], training = make_train(.7, n(), ))

mutate(r_data[[dataset]], training = make_train(.7, n(), attr(group_by(r_data[[dataset]], gear, am), "indices")))
r_data[[dataset]] %>% mutate(training = make_train(.7, n(), attr(group_by(., gear, am), "indices")))

group_by(r_data[[dataset]], gear, am) %>% {make_train(.7, nrow(.), attr(., "indices"))}

make_train <- function(n = .7, nr = 100, grps = list()) {
  nr_grps <- length(grps)
  print(nr_grps)
  training <- rep_len(0L,nr)
  if (nr_grps == 0) {
    if (n < 1) n <- round(n * nr) %>% max(1)
    # ind <- seq_len(nr)
    # training <- rep_len(0L,nr)
    training[sample(seq_len(nr),n)] <- 1L
  } else {
    if (n < 1) n <- round(n * nr_grps) %>% max(1)
    tgrp <- make_train(n, nr_grps)
    # training <- make_train(n, nr_grps)
    # training <- rep_len(0L,nr)
    for (i in 1:nr_grps) {
      training[grps[[i]]] <- tgrp[i]
    }
  }
  training
}

mutate(r_data[[dataset]], training = make_train(.7, n(), .))

make_train <- function(n = .7, nr = 100, dat = data.frame(), var = "") {

  # nr_grps <- length(grps)
  # distinct(select(mtcars, cyl))

  training <- rep_len(0L,nr)
  if (nr_grps == 0) {
    if (n < 1) n <- round(n * nr) %>% max(1)
    # ind <- seq_len(nr)
    # training <- rep_len(0L,nr)
    training[sample(seq_len(nr),n)] <- 1L
  } else {
    if (n < 1) n <- round(n * nr_grps) %>% max(1)
    tgrp <- make_train(n, nr_grps)
    # training <- make_train(n, nr_grps)
    # training <- rep_len(0L,nr)
    for (i in 1:nr_grps) {
      training[grps[[i]]] <- tgrp[i]
    }
  }
  training
}

r_data[[dataset]] %>% mutate(training = make_train(.7, n(), ., "mpg"))

group_by(r_data[[dataset]], gear, am) %>% {make_train(.7, nrow(.), attr(., "indices"))}


mtcars %>% mutate(training = gear %in% sample(unique(gear), 1))


mtcars %>% mutate(training = as.integer(gear %in% sample(unique(gear), 2)))

mtcars %>% mutate_each_(funs(training = {as.integer(. %in% sample(unique(.), 2))}), vars = "gear")
mtcars %>% mutate_each_(funs(as.integer(. %in% sample(unique(.), 2))), vars = "gear")


mtcars %>% mutate_each_(funs(as.integer(. %in% sample(unique(.), 2))), vars = "gear")






# make_train <- function(n = .7, nr = 100, grps = list()) {
#   nr_grps <- length(grps)
#   print(nr_grps)
#   training <- rep_len(0L,nr)
#   if (nr_grps == 0) {
#     if (n < 1) n <- round(n * nr) %>% max(1)
#     # ind <- seq_len(nr)
#     # training <- rep_len(0L,nr)
#     training[sample(seq_len(nr),n)] <- 1L
#   } else {
#     if (n < 1) n <- round(n * nr_grps) %>% max(1)
#     tgrp <- make_train(n, nr_grps)
#     # training <- make_train(n, nr_grps)
#     # training <- rep_len(0L,nr)
#     for (i in 1:nr_grps) {
#       training[grps[[i]]] <- tgrp[i]
#     }
#   }
#   training
# }

# group_by(r_data[[dataset]], gear, am) %>% {make_train(.7, nrow(.), attr(., "indices"))}

# ?row_number()

# r_data <- list()
# r_data$dat <- mtcars

# group_by(r_data[[dataset]], gear) %>% mutate(group = )

#          %>% {make_train(.7, nrow(.), attr(., "indices"))}



# a <- rep(c("a","b"), each =3)
# b <- c(4,4,5,11,12,13)
# foo <- data.frame(a,b, stringsAsFactors = F)

# # foo %>% group_by(a) %>% mutate(dense_rank(a))

# # foo %>% group_by(a) %>% mutate(row_number())
# foo %>% group_by(a) %>% mutate(group = cumsum(c(1, lag(.))))

# foo %>% group_by(a) %>% mutate(group = cumsum(c(1, lag(a))))

# group_indices(mtcars, cyl, vs)

# .training <- function(dataset,
#                       n = .7,
#                       nr = 100,
#                       name = "training",
#                       store_dat = "",
#                       store = TRUE) {

#   if (is_empty(name)) name <- "training"
#   if (!store && !is.character(dataset)) {
#     n <- n %>% {ifelse (. < 0 || is.na(.) || . > nr, .7, .)}
#     data.frame(make_train(n, nr)) %>% setNames(name)
#   } else {
#     if (store_dat == "") store_dat <- dataset
#     paste0("## created variable to select training sample\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", name, " = make_train(", n, ", n()))\n")
#   }
# }
