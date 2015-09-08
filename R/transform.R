#' Center
#' @param x Input variable
#' @return If x is a numberic variable return x - mean(x)
#' @export
center <- function(x)
	if (is.numeric(x)) { x - mean(x, na.rm = TRUE) } else x

#' Standardize
#' @param x Input variable
#' @return If x is a numberic variable return center(x) / mean(x)
#' @export
standardize <- function(x)
	if (is.numeric(x)) { center(x) / sd_rm(x) } else x

#' Median split
#' @param x Input variable
#' @return Factor variable deciles

#' @export
median_split <- function(x) {
  cut(x, breaks = quantile(x,c(0,.5,1)),
      include.lowest = TRUE,
      labels = c("Below", "Above"))
}

#' Create deciles
#' @param x Input variable
#' @return Factor variable
#' @export
decile_split <- function(x) {
  ## avoid non-unique breaks
  df <- data.frame(breaks = quantile(x, seq(0,1,.1))) %>% set_rownames(0:10) %>% unique
  cut(x, df$breaks, rownames(df)[-1], include.lowest = TRUE)
}

#' Calculate square of a variable
#' @param x Input variable
#' @return x^2
#' @export
square <- function(x) x^2

#' Calculate inverse of a variable
#' @param x Input variable
#' @return 1/x
#' @export
inverse <- function(x) 1/x

#' Normalize a variable x by a variable y
#' @param x Input variable
#' @param y Normalizing variable
#' @return x/y
#' @export
normalize <- function(x,y) x/y

#' Convert input in month-day-year format to date
#' @details Use as.character if x is a factor
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_mdy("2-1-2014")
#' \dontrun{
#' as_mdy("2-1-2014") %>% month(label = TRUE)
#' as_mdy("2-1-2014") %>% week
#' as_mdy("2-1-2014") %>% wday(label = TRUE)
#' }
#' @export
as_mdy <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
            {sshhr( mdy(.) )} %>% as.Date

#' Convert input in day-month-year format to date
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_dmy("1-2-2014")
#'
#' @export
as_dmy <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
            {sshhr( dmy(.) )} %>% as.Date

#' Convert input in year-month-day format to date
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_ymd("2013-1-1")
#'
#' @export
as_ymd <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
            {sshhr( ymd(.) )} %>% as.Date

# http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html
#' Convert input in year-month-day-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_ymd_hms("2014-1-1 12:15:01")
#' \dontrun{
#' as_ymd_hms("2014-1-1 12:15:01") %>% as.Date
#' as_ymd_hms("2014-1-1 12:15:01") %>% month
#' as_ymd_hms("2014-1-1 12:15:01") %>% hour
#' }
#' @export
as_ymd_hms <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
                {sshhr( ymd_hms(.) )}

#' Convert input in hour-minute-second format to time
#' @param x Input variable
#' @return Time variable of class Period
#' @examples
#' as_hms("12:45:00")
#' \dontrun{
#' as_hms("12:45:00") %>% hour
#' as_hms("12:45:00") %>% second
#' }
#' @export
as_hms <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
            {sshhr( hms(.) )}

#' Convert input in hour-minute format to time
#' @param x Input variable
#' @return Time variable of class Period
#' @examples
#' as_hm("12:45")
#' \dontrun{
#' as_hm("12:45") %>% minute
#' }
#' @export
as_hm <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
            {sshhr( hm(.) )}

#' Convert variable to integer avoiding potential issues with factors
#' @param x Input variable
#' @return Integer
#' @examples
#' as_integer(rnorm(10))
#' as_integer(letters)
#' as_integer(5:10 %>% as.factor)
#' as.integer(5:10 %>% as.factor)
#'
#' @export
as_integer <- function(x) {
	if (is.factor(x)) {
		levels(x) %>% .[x] %>% as.integer
	} else if (is.character(x)) {
    x
  } else {
		as.integer(x)
	}
}

#' Convert variable to numeric avoiding potential issues with factors
#' @param x Input variable
#' @return Numeric
#' @examples
#' as_numeric(rnorm(10))
#' as_numeric(letters)
#' as_numeric(5:10 %>% as.factor)
#' as.numeric(5:10 %>% as.factor)
#'
#' @export
as_numeric <- function(x) {
	if (is.factor(x)) {
		levels(x) %>% .[x] %>% as.numeric
  } else if (is.character(x)) {
    x
	} else {
    as.numeric(x)
	}
}

#' Wrapper for as.factor
#' @param x Input vector
#' @export
as_factor <- function(x) as.factor(x)

#' Wrapper for as.character
#' @param x Input vector
#' @export
as_character <- function(x) as.character(x)

#' Generate a variable used to selected a training sample
#' @param n Number (or fraction) of observations to label as training
#' @param nr Number of rows in the dataset
#' @return 0/1 variables for filtering
#' @examples
#' make_train(.5, 10)
#'
#' @export
make_train <- function(n = .7, nr = 100) {
  if (n < 1) n <- round(n * nr) %>% max(1)
  ind <- seq_len(nr)
  training <- rep_len(0L,nr)
  training[sample(ind,n)] <- 1L
  training
}

#' Filter out rows with missing observations
#'
#' @details Filter data on complete cases for selected variables
#'
#' @param data Data.frame to filter
#' @param vars Variables used to identify rows with missing values (default is all)
#'
#' @examples
#' mtcars[1:4, "mpg"] <- NA; mtcars[5:10, "carb"] <- NA
#' filter_na(mtcars, "mpg")
#'
#' @export
filter_na <- function(data, vars = "") {
  if (all(vars == "")) return(na.omit(data))
  ind <- select_(data, .dots = vars) %>% complete.cases
  filter(data, ind)
}

#' Add tranformed variables to a data.frame (SE)
#'
#' @param data Data.frame to add transformed variables to
#' @param fun Function to apple (e.g., "log")
#' @param ext Extension to add for each variable
#' @param vars Variables to transform (default is all)
#'
#' @examples
#' mutate_each_add_(mtcars, fun = "log", ext = "_log", vars = c("mpg","cyl"))
#'
#' @export
mutate_each_add_ <- function(data, fun,
                             ext = "",
                             vars = "") {

    fun <- get(fun)
    data %>% select_(.dots = vars) %>% mutate_each_(funs(fun), vars) %>%
      set_colnames(paste0(vars, ext)) %>% bind_cols(data, .)
}


#' Add tranformed variables to a data.frame (NSE)
#'
#' @param data Data.frame to add transformed variables to
#' @param fun Function to apple (e.g., "log")
#' @param ext Extension to add for each variable
#' @param ... Variables to transform
#'
#' @examples
#' mutate_each_add(mtcars, fun = "log", ext = "_log", mpg, cyl)
#'
#' @importFrom pryr named_dots
#'
#' @export
mutate_each_add <- function(data, fun,
                            ext = "",
                            ...) {

    vars <- pryr::named_dots(...) %>% names
    if (is.null(vars)) vars <- colnames(data)

    fun <- get(fun)
    # data %>% select(...) %>% mutate_each(funs(fun), ...) %>%
    data %>% select_(.dots = vars) %>% mutate_each_(funs(fun), vars = vars) %>%
      set_colnames(paste0(vars, ext)) %>% bind_cols(data, .)

# data %>%  mutate_each(funs(fun), ...) %>% select(...) %>%
#   set_colnames(paste0(vars, ext)) %>% bind_cols(data, .)

}

#' Add normalized variables to a data.frame (NSE)
#'
#' @param data Data.frame to add transformed variables to
#' @param ext Extension to add for each variable
#' @param ... Variables to normalize. The first variable is used as the normalizer
#'
#' @examples
#' normalize_each_add(mtcars, ext = "_nz", mpg, cyl, mpg)
#'
#' @importFrom pryr named_dots
#'
#' @export
normalize_each_add <- function(data, ext = "", ...) {

  vars <- pryr::named_dots(...) %>% names
  nz <- select_(data, vars[1])[[1]]
  vars <- vars[-1]

  nlize <- function(x) x / nz

  data %>% select_(.dots = vars) %>%
    mutate_each_(funs(nlize(.)), vars = vars) %>%
    set_colnames(paste0(vars, ext)) %>% bind_cols(data, .)
}

# Create new variables and add them to the dataframe
#
# @param data Data.frame to add transformed variables to
# @param cmd Expression that will generate the new variable(s). To create multiple new variables separate commands using a ";"
#
# @examples
# create(mtcars, "mpg0 = mpg - mean(mpg); mpg_ln = log(mpg)")
#
# @export
# create <- function(data, cmd) {
#   do.call(within, list(dataset, parse(text = cmd)))
# }

## Test
# dat <- read.table(header = TRUE, text = "date days
# 1/1/10  1
# 1/2/10  2
# 1/3/10  3
# 1/4/10  4
# 1/5/10  5
# 1/6/10  6
# 1/7/10  7
# 1/8/10  8
# 1/9/10  9
# 1/10/10 10")
# sapply(dat,class)
# library(lubridate)
# library(magrittr)
# dat$date %>% as_mdy %T>% print %>% class
# dat$date %<>% as.character
# dat$date %>% as_mdy %T>% print %>% class
# dat$date %<>% as.factor
# dat$date %>% as_mdy %T>% print %>% class

## time in hours:minutes and seconds
# time <- "19:12:01"
# time %<>% { if (is.factor(.)) as.character(.) else . } %>% lubridate::hms(.)


# ?hours

# time <- "19:12"
# time %<>% { if (is.factor(.)) as.character(.) else . } %>% lubridate::hm(.)
# time
# ?hm
# hours(time)
# minutes(time)
# seconds(time)

#' Create data.frame summary
#'
#' @details Used in Radiant's Data > Transform tab
#'
#' @param dat Data.frame
#' @param dc Class for each variable
#'
#' @export
getsummary <- function(dat, dc = getclass(dat)) {

  isFct <- "factor" == dc
  isNum <- "numeric" == dc | "integer" == dc
  isDate <- "date" == dc
  isChar <- "character" == dc
  isLogic <- "logical" == dc
  isPeriod <- "period" == dc

  if (sum(isNum) > 0) {

    cn <- names(dc)[isNum]

    cat("Summarize numeric variables:\n")
    select(dat, which(isNum)) %>%
      tidyr::gather_("variable", "values", cn) %>%
      group_by_("variable") %>%
      summarise_each(funs(n = length, n_missing = n_missing, n_distinct = n_distinct,
                     mean = mean_rm, median = median_rm, min = min_rm, max = max_rm,
                     `25%` = p25, `75%` = p75, sd = sd_rm, se = serr, cv = sd/mean)) %>%
      data.frame(check.names = FALSE) %>%
      { .[,-1] %<>% round(.,3); colnames(.)[1] <- ""; . } %>%
      print(row.names = FALSE)
    cat("\n")
  }
  if (sum(isFct) > 0) {
    cat("Summarize factors:\n")
    select(dat, which(isFct)) %>% summary(maxsum = 12) %>% print
    cat("\n")
  }
  if (sum(isDate) > 0) {
    cat("Earliest dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(min)) %>% print
    cat("\nFinal dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(max)) %>% print
    cat("\n")
  }
  if (sum(isPeriod) > 0) {
    cat("Earliest time:\n")
    select(dat, which(isPeriod)) %>% summarise_each(funs(min)) %>% print
    cat("\nFinal time:\n")
    select(dat, which(isPeriod)) %>% summarise_each(funs(max)) %>% print
    cat("\n")
  }
  if (sum(isChar) > 0) {
    cat("Summarize character variables (< 20 unique values shown):\n")
    select(dat, which(isChar)) %>% distinct %>% lapply(unique) %>%
      {for(i in names(.)) cat(i, ":", .[[i]][1:min(20,length(.[[i]]))], "\n")}
    cat("\n")
  }
  if (sum(isLogic) > 0) {
    cat("Summarize logical variables:\n")
    select(dat, which(isLogic)) %>% summarise_each(funs(sum)) %>%
      as.data.frame %>% set_rownames("# True") %>% print
    cat("\n")
  }
}



