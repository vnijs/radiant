nmissing <- function(x) sum(is.na(x))
p25 <- function(x, na.rm = TRUE) quantile(x,.25, na.rm = na.rm)
p75 <- function(x, na.rm = TRUE) quantile(x,.75, na.rm = na.rm)
serr <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / length(na.omit(x))
cv <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
.mean <- function(x) mean(x, na.rm = TRUE)
.median <- function(x) median(x, na.rm = TRUE)
.min <- function(x) min(x, na.rm = TRUE)
.max <- function(x) max(x, na.rm = TRUE)
.sd <- function(x) sd(x, na.rm = TRUE)

getsummary <- function(dat, dc = getclass(dat)) {

  isFct <- "factor" == dc
  isNum <- "numeric" == dc | "integer" == dc
  isDate <- "date" == dc | "Date" == dc
  isChar <- "character" == dc
  isLogic <- "logical" == dc

  if(sum(isNum) > 0) {

    cn <- names(dc)[isNum]

    cat("Summarize numeric variables:\n")
    select(dat, which(isNum)) %>%
      tidyr::gather_("variable", "values", cn) %>%
      group_by(variable) %>%
      summarise_each(funs(n = length, missing = nmissing, mean = .mean,
                     median = .median, min = .min, max = .max,
                     `25%` = p25, `75%` = p75, sd = .sd, se = serr,
                     cv = sd/mean)) %>%
      data.frame(check.names = FALSE) %>%
      { .[,-1] %<>% round(.,3); colnames(.)[1] <- ""; . } %>%
      print(row.names = FALSE)
    cat("\n")
  }
  if(sum(isFct) > 0) {
    cat("Summarize factors:\n")
    select(dat, which(isFct)) %>% summary %>% print
    cat("\n")
  }
  if(sum(isDate) > 0) {
    cat("Earliest dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(min)) %>% print
    cat("\nFinal dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(max)) %>% print
    cat("\n")
  }
  if(sum(isChar) > 0) {
    cat("Summarize character variables:\n")
    select(dat, which(isChar)) %>% table %>% print
    cat("\n")
  }
  if(sum(isLogic) > 0) {
    cat("Summarize logical variables:\n")
    select(dat, which(isLogic)) %>% tally %>% print
    cat("\n")
  }
}

explore <- function(dataset, expl_vars = "",
                    data_filter = "",
                    expl_byvar = c(),
                    expl_function = c("n" = "length", "mean" = ".mean")) {

  # library(radiant)
  # dat <- .getdata()
  # dataset <- "mtcars"
  # data_filter <- ""
  # expl_vars <- ""
  # expl_byvar <- c()
  # expl_function = c("n" = "length", "mean" = ".mean")
  vars <- c(expl_vars, expl_byvar)

  dat <- getdata(dataset, vars, filt = data_filter)

  if(is.null(expl_byvar)) {
    res <- capture.output(getsummary(dat))
  } else {

    res <- list()
    dc <- getclass(dat)
    isNum <- "numeric" == dc | "integer" == dc
    dat %<>% group_by_(.dots = expl_byvar) %>% select(which(isNum))
    for(f in expl_function) {
      gf <- get(f)
      res[[f]] <- dat %>% summarise_each(funs(gf)) %>% as.data.frame
    }
  }

  # dat no longer needed
  rm(dat)

  environment() %>% as.list %>% set_class(c("explore",class(.)))
}

summary.explore <- function(object, ...) {

  cat("Data     :", object$dataset, "\n")
  if(object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter   :", gsub("\\n","", object$data_filter), "\n")

  cat("\n")
  if(class(object$res) == "character") {
    cat(paste0(object$res,sep="\n"))
  } else {
    for(f in object$expl_function) {
      cat("Results grouped by: ", object$expl_byvar, "\n")
      cat("Function used: ", names(which(expl_functions == f)), "\n")
      object$res[[f]] %>%
        { .[,-c(1:length(expl_byvar))] %<>% round(3); . } %>%
        print
      cat("\n")
    }
  }
}

plot.explore <- function(x, shiny = FALSE, ...) {

  if(class(object$res) == "character")
    return(invisible())

  object <- x; rm(x)

  by_var <- fill_var <- object$expl_byvar[1]
  if(length(object$expl_byvar) > 1) fill_var <- object$expl_byvar[2]
  if(object$expl_vars == "")
    object$expl_vars <- names(object$res[[1]])[-c(1:length(object$expl_byvar))]

  plots <- list()
  for(f in object$expl_function) {
    for(var in object$expl_vars) {
      plots[[paste0(var,"_",f)]] <-
        ggplot(data = object$res[[f]], aes_string(x = by_var, y = var, fill = fill_var)) +
          geom_bar(stat="identity", position = "dodge", alpha=.7) +
          ggtitle(paste("Function used:", names(which(expl_functions == f))))

      if(length(object$expl_byvar) == 1) {
        plots[[paste0(var,"_",f)]] <- plots[[paste0(var,"_",f)]] +
          theme(legend.position = "none")
      }
    }
  }

  sshhr( do.call(gridExtra::arrangeGrob, c(plots, list(ncol = 1))) ) %>%
    { if(shiny) . else print(.) }
}

# dat <- mtcars
# dat$names <- rownames(mtcars)
# dat$vs %<>% as.factor
# dat$am %<>% as.factor
# getsummary(dat)
# object <- explore("dat")
# summary(object)
# explore("dat") %>% summary
# explore("dat", expl_vars = "mpg", expl_byvar = "vs") %>% summary
# explore("dat", expl_vars = "mpg", expl_byvar = "vs") %>% plot
# explore("dat", expl_vars = "mpg", expl_byvar = c("vs","am")) %>% plot
# object <- explore("dat", expl_byvar = "vs")
# explore("dat", expl_byvar = c("vs","am")) %>% plot
# explore("dat", expl_byvar = "vs") %>% summary
# explore("dat", data_filter = "mpg > 20") %>% summary
# explore("dat", data_filter = "mpg > 20") %>% plot
