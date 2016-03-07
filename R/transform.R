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

#' Calculate square of a variable
#' @param x Input variable
#' @return x^2
#' @export
square <- function(x) x^2

#' Calculate inverse of a variable
#' @param x Input variable
#' @return 1/x
#' @export
inverse <- function(x) {
  stopifnot(y != 0)
  1/x
}

#' Normalize a variable x by a variable y
#' @param x Input variable
#' @param y Normalizing variable
#' @return x/y
#' @export
normalize <- function(x,y) {
  stopifnot(y != 0)
  x/y
}

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

#' Convert input in year-month-day-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_ymd_hm("2014-1-1 12:15")
#' @export
as_ymd_hm <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
                {sshhr( parse_date_time(x, "%Y%m%d %H%M") )}

#' Convert input in month-day-year-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hms("1-1-2014 12:15:01")
#' @export
as_mdy_hms <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
                {sshhr( parse_date_time(x, "%m%d%Y %H%M%S") )}

#' Convert input in month-day-year-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hm("1-1-2014 12:15")
#' @export
as_mdy_hm <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
                {sshhr( parse_date_time(x, "%m%d%Y %H%M") )}

#' Convert input in day-month-year-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hms("1-1-2014 12:15:01")
#' @export
as_dmy_hms <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
                {sshhr( parse_date_time(x, "%d%m%Y %H%M%S") )}

#' Convert input in day-month-year-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hm("1-1-2014 12:15")
#' @export
as_dmy_hm <- function(x) { if (is.factor(x)) as.character(x) else x } %>%
                {sshhr( parse_date_time(x, "%d%m%Y %H%M") )}

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
    int <- sshhr( try(as.integer(x), silent = TRUE))
    if (length(na.omit(int)) == 0) x else int
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
#' as_numeric(c("1","2"))
#'
#' @export
as_numeric <- function(x) {
	if (is.factor(x)) {
		levels(x) %>% .[x] %>% as.numeric
  } else if (is.character(x)) {
    num <- sshhr( try(as.numeric(x), silent = TRUE))
    if (length(na.omit(num)) == 0) x else num
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

#' Wrapper for lubridate's as.duration function. Result converted to numeric
#' @param x Time difference
#' @export
as_duration <- function(x) as.numeric(lubridate::as.duration(x))

#' Distance in kilometers or miles between two locations based on lat-long
#' Function based on \url{http://www.movable-type.co.uk/scripts/latlong.html}. Uses the haversine formula
#' @param long1 Longitude of location 1
#' @param lat1 Latitude of location 1
#' @param long2 Longitude of location 2
#' @param lat2 Latitude of location 2
#' @param unit Measure kilometers ("km", default) or miles ("miles")
#' @param R Radius of the earth
#' @return Distance bewteen two points
#' @examples
#' as_distance(32.8245525,-117.0951632, 40.7033127,-73.979681, unit = "km")
#' as_distance(32.8245525,-117.0951632, 40.7033127,-73.979681, unit = "miles")
#'
#' @export
as_distance <- function (lat1, long1, lat2, long2,
                         unit = "km",
                         R = c("km" = 6371, "miles" = 3959)[[unit]]) {

    rad <- pi/180
    d1 <- lat1 * rad
    d2 <- lat2 * rad
    dlat <- (lat2 - lat1) * rad
    dlong <- (long2 - long1) * rad
    a <- sin(dlat/2)^2 + cos(d1) * cos(d2) * sin(dlong/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R * c
}

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

#' Add tranformed variables to a data frame (NSE)
#'
#' @details Wrapper for dplyr::mutate_each that allows custom variable name extensions
#'
#' @param tbl Data frame to add transformed variables to
#' @param funs Function(s) to apply (e.g., funs(log))
#' @param ... Variables to transform
#' @param ext Extension to add for each variable
#'
#' @examples
#' mutate_each(mtcars, funs(log), mpg, cyl, ext = "_log")
#'
#' @importFrom pryr named_dots
#'
#' @export
mutate_each <- function(tbl, funs, ..., ext = "") {

  if (ext == "") {
    dplyr::mutate_each(tbl, funs, ...)
  } else {
    vars <- pryr::named_dots(...) %>% names
    if (is.null(vars)) vars <- colnames(tbl)

    new <- paste0(vars, ext)
    tbl[,new] <-
      tbl %>% mutate_each_(funs, vars = vars) %>% select_(.dots = vars) %>%
      set_colnames(new)
    tbl
  }
}

#' Create a quintile (or decile) index
#'
#' @details Same as stata
#'
#' @param x Numeric variable
#' @param n number of bins to create
#' @param rev Reverse the order of the xtiles
#'
#' @examples
#' xtile(1:10,5)
#' xtile(1:10,5, rev = TRUE)
#'
#' @export
xtile <- function(x, n, rev = FALSE) {
  stopifnot(is.numeric(n), is.numeric(x), n > 1, length(x) > n)
  breaks <- quantile(x, prob = seq(0, 1, length = n+1), type = 2)
  if (length(breaks) < 2) stop(paste("Insufficient variation in x to construct",n,"breaks"))
  .bincode(x, breaks, include.lowest = TRUE) %>%
  { if (rev) as.integer((n+1) - .) else .}
}

#' Show all rows with duplicated values (not just the first or last)
#'
#' @details If an entire row is duplicated use "duplicated" to show only one of the duplicated rows. When using a subset of variables to establish uniqueness it may be of interest to show all rows that have (some) duplicate elements
#'
#' @param tbl Data frame to add transformed variables to
#' @param ... Variables used to evaluate row uniqueness
#'
#' @examples
#' bind_rows(mtcars, mtcars[c(1,5,7),]) %>%
#'   show_duplicated(mpg, cyl)
#' bind_rows(mtcars, mtcars[c(1,5,7),]) %>%
#'   show_duplicated
#'
#' @importFrom pryr named_dots
#'
#' @export
show_duplicated <- function(tbl, ...) {
  vars <- pryr::named_dots(...) %>% names
  if (is.null(vars) || length(unique(vars)) == ncol(tbl)) {
    filter(tbl, duplicated(tbl))
  } else {
    tbl %>% group_by_(.dots = vars) %>%
      filter(n() > 1) %>%
      mutate(nr_dup = 1:n()) %>%
      arrange_(.dots = vars) %>%
      ungroup
  }
}

#' Weighted standard deviation
#'
#' @details Calculated a weighted standard deviation
#'
#' @param x Numeric vector
#' @param wt Numeric vector of weights
#' @param na.rm Remove missing values (default is TRUE)
#'
#' @export
weighted.sd <- function(x, wt, na.rm = TRUE) {
  if (na.rm) {
    x <- na.omit(x)
    wt <- na.omit(wt)
  }
  wt <- wt/sum(wt)
  wm <- weighted.mean(x, wt)
  sqrt(sum(wt * (x - wm)^2))
}

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
  isNum <- "numeric" == dc | "integer" == dc | "Duration" == dc
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
                     `25%` = p25, `75%` = p75, sd = sd_rm, se = serr)) %>%
      data.frame(check.names = FALSE) %>%
      { .[,-1] %<>% round(.,3); colnames(.)[1] <- ""; . } %>%
      print(row.names = FALSE)
    cat("\n")
  }

  if (sum(isFct) > 0) {
    cat("Summarize factors:\n")
    select(dat, which(isFct)) %>% summary(maxsum = 20) %>% print
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

    max_time <- function(x) sort(x) %>% tail(1)
    min_time <- function(x) sort(x) %>% head(1)

    cat("Earliest time:\n")
    select(dat, which(isPeriod)) %>% summarise_each(funs(min_time)) %>% print
    cat("\nFinal time:\n")
    select(dat, which(isPeriod)) %>% summarise_each(funs(max_time)) %>% print
    cat("\n")
  }

  if (sum(isChar) > 0) {
    ## finding unique elements can be slow for large files
    if (nrow(dat) < 10^5) {
      cat("Summarize character variables (< 20 unique values shown):\n")
      select(dat, which(isChar)) %>% lapply(unique) %>%
        {for(i in names(.)) cat(i, paste0("(n_distinct ", length(.[[i]]), "): "), .[[i]][1:min(20,length(.[[i]]))], "\n")}
    } else {
      cat("Summarize character variables (< 20 values shown):\n")
      select(dat, which(isChar)) %>%
        {for(i in names(.)) cat(i, ":", .[[i]][1:min(20,length(.[[i]]))], "\n")}
    }
    cat("\n")
  }
  if (sum(isLogic) > 0) {
    cat("Summarize logical variables:\n")
    select(dat, which(isLogic)) %>% summarise_each(funs(sum, mean)) %>% matrix(ncol = 2) %>%
      set_colnames(c("# TRUE", "% TRUE")) %>% set_rownames(names(dat)[isLogic]) %>% print
    cat("\n")
  }
}

#' Create data.frame from a table
#'
#' @param dat Data.frame
#' @param freq Column name with frequency information
#'
#' @examples
#' data.frame(price = c("$200","$300"), sale = c(10, 2)) %>% table2data
#'
#' @export
table2data <- function(dat, freq = tail(colnames(dat),1)) {
  blowup <- function(i)
    if (!is.na(dat[[freq]][i])) dat[rep(i, each = dat[[freq]][i]), ]

  lapply(1:nrow(dat), blowup) %>%
  bind_rows %>%
  select_(paste0("-",freq)) %>%
  mutate_each(funs(as.factor))
}

#' Generate list of levels and unique values
#'
#' @param dat A data.frame
#' @param ... Unquoted variable names to evaluate
#'
#' @examples
#' data.frame(a = c(rep("a",5),rep("b",5)), b = c(rep(1,5),6:10)) %>% level_list
#' level_list(mtcars, mpg, cyl)
#'
#' @importFrom pryr named_dots
#'
#' @export
level_list <- function(dat, ...) {
  fl <- function(x) {
    cx <- class(x)[1]
    if (cx == "factor") {
      levels(x)
    } else {
      unique(x)
    }
  }
  dots <- pryr::named_dots(...) %>% names
  if (length(dots) > 0) {
    lapply(select_(dat, .dots = dots), fl)
  } else {
    lapply(dat, fl)
  }
}

# dat <-
#   data.frame(
#     free_ship = c("$200","$300","$200","$300"),
#     sale = c("yes","no","yes","no"),
#     freq = c(3, 5,2, 4)
#   )

# dat <-
#   data.frame(
#     free_ship = c("$200","$300","$200","$300"),
#     sale = c("yes","no","yes","no"),
#     freq = c(500, 9500,580, 9420)
#   )

# table2data(dat)

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
# library(lubridate)
# time <- as_hms("19:12:01") + lubridate::minutes(0:2999) %>% data.frame
# summarise_each(time, funs(max))
# max(minute(time[[1]]))
# time(time)
# arrange(time) %>% tail(1)
# arrange(time) %>% tail(1)
# time %<>% { if (is.factor(.)) as.character(.) else . } %>% lubridate::hms(.)
# time <- "19:12"
# time %<>% { if (is.factor(.)) as.character(.) else . } %>% lubridate::hm(.)
# time
# ?hm
# hours(time)
# minutes(time)
# seconds(time)


# library(lubridate)
# library(DT)
# dat <- read.table(header = TRUE, text = "time
# 1:00
# 1:01
# 1:02
# 1:03
# 1:04
# 1:05
# 1:06
# 1:07
# 4:08
# 8:09
# 9:10")

# dat$time <- hm(dat$time)
# class(dat$time)

# datatable(dat, filter = 'top')

