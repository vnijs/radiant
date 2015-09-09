
# #' Add tranformed variables to a data.frame (SE)
# #'
# #' @param tbl Data.frame to add transformed variables to
# #' @param fun Function to apple (e.g., "log")
# #' @param ext Extension to add for each variable
# #' @param vars Variables to transform (default is all)
# #'
# #' @examples
# #' mutate_each_add_(mtcars, fun = "log", ext = "_log", vars = c("mpg","cyl"))
# #'
# #' @export
# mutate_each_add_ <- function(tbl, fun,
#                              ext = "",
#                              vars = "") {

#     fun <- get(fun)
#     tbl %>% select_(.dots = vars) %>% mutate_each_(funs(fun), vars) %>%
#       set_colnames(paste0(vars, ext)) %>% bind_cols(tbl, .)
# }


# #' Add tranformed variables to a data.frame (NSE)
# #'
# #' @param tbl Data.frame to add transformed variables to
# #' @param fun Functions to apple (e.g., "log")
# #' @param ext Extension to add for each variable
# #' @param ... Variables to transform
# #'
# #' @examples
# #' mutate_each_add(mtcars, fun = "log", ext = "_log", mpg, cyl)
# #'
# #' @importFrom pryr named_dots
# #'
# #' @export
# mutate_each_add <- function(tbl, fun,
#                             ext = "",
#                             ...) {

#     vars <- pryr::named_dots(...) %>% names
#     if (is.null(vars)) vars <- colnames(tbl)

#     fun <- get(fun)
#     tbl %>% select_(.dots = vars) %>% mutate_each_(funs(fun), vars = vars) %>%
#       set_colnames(paste0(vars, ext)) %>% bind_cols(tbl, .)

# }



# filter_na <- function(data, vars = "") {
#   # vars <- pryr::named_dots(...) %>% names
#   if (all(vars == "")) return(na.omit(data))
#   ind <- select_(data, .dots = vars) %>% complete.cases
#   filter(data, ind)
# }


# #' Add normalized variables to a data.frame (NSE)
# #'
# #' @details Wrapper for mutate_each. The mutate_each example show why mutate each cannot be used directly (i.e., table is treated as a function rather than a variable)
# #'
# #' @param tbl Data frame to add transformed variables to
# #' @param ext Extension to add for each variable
# #' @param ... Variables to normalize. The first variable is used as the normalizer
# #'
# #' @examples
# #' normalize_each(mtcars, ext = "_nz", mpg, cyl, mpg)
# #' normalize_each(diamonds, ext = "_pt", table, price, carat)
# #' \dontrun{
# #' mutate_each(diamonds, funs(./table), ext = "_pt", price, carat)
# #' }
# #'
# #' @importFrom pryr named_dots
# #'
# #' @export
# normalize_each <- function(tbl, ext = "", ...) {

#   vars <- pryr::named_dots(...) %>% names
#   nz <- select_(tbl, vars[1]) %>% .[[1]]
#   vars <- vars[-1]

#   # normalize <- function(x) x / nz
#   # mutate_each_(funs(nlize(.)), vars = vars) %>%

#   tbl %>% select_(.dots = vars) %>%
#     mutate_each_(funs(normalize(., nz)), vars = vars) %>%
#     set_colnames(paste0(vars, ext)) %>% bind_cols(tbl, .)
# }


# #' Filter out rows with missing observations (NSE)
# #'
# #' @details Filter data on complete cases for selected variables
# #'
# #' @param tbl Data frame to filter
# #' @param ... Variables used to identify rows with missing values (default is all)
# #'
# #' @examples
# #' mtcars[1:4, "mpg"] <- NA; mtcars[5:10, "carb"] <- NA
# #' filter_na(mtcars, mpg)
# #'
# #' @importFrom pryr named_dots
# #'
# #' @export
# filter_na <- function(tbl, ...) {
#   vars <- pryr::named_dots(...) %>% names
#   if (is.null(vars) || all(vars == "\"\"")) return(na.omit(tbl))
#   ind <- select_(tbl, .dots = vars) %>% complete.cases
#   filter(tbl, ind)
# }

# #' Filter out rows with missing observations (SE)
# #'
# #' @details Filter data on complete cases for selected variables
# #'
# #' @param tbl Data frame to filter
# #' @param vars Variables used to identify rows with missing values (default is all)
# #'
# #' @examples
# #' mtcars[1:4, "mpg"] <- NA; mtcars[5:10, "carb"] <- NA
# #' filter_na_(mtcars, c("mpg","carb"))
# #'
# #' @importFrom pryr named_dots
# #'
# #' @export
# filter_na_ <- function(tbl, vars = "") {
#   if (is.null(vars) || all(vars == "")) return(na.omit(tbl))
#   ind <- select_(tbl, .dots = vars) %>% complete.cases
#   filter(tbl, ind)
# }
