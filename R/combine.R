#' Combine datasets using dplyr's bind and join functions
#'
#' @details See \url{http://vnijs.github.io/radiant/base/combine.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param cmb_dataset Dataset name (string) to combine with `dataset`. This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param by Variables used to combine `dataset` and `cmb_dataset`
#' @param type The main bind and join types from the dplyr package are provided. \bold{inner_join} returns all rows from x with matching values in y, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. \bold{left_join} returns all rows from x, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. \bold{right_join} is equivalent to a left join for datasets y and x. \bold{full_join} combines two datasets, keeping rows and columns that appear in either. \bold{semi_join} returns all rows from x with matching values in y, keeping just columns from x. A semi join differs from an inner join because an inner join will return one row of x for each matching row of y, whereas a semi join will never duplicate rows of x. \bold{anti_join} returns all rows from x without matching values in y, keeping only columns from x. \bold{bind_rows} and \bold{bind_cols} are also included, as are \bold{intersect}, \bold{union}, and \bold{setdiff}. See \url{http://vnijs.github.io/radiant/base/combine.html} for further details
#' @param name Name for the combined dataset
#'
#' @return If list `r_data` exists the combined dataset is added as `name`. Else the combined dataset will be returned as `name`
#'
#' @examples
#' combinedata("titanic","titanic_pred",c("pclass","sex","age")) %>% head
#' titanic %>% combinedata("titanic_pred",c("pclass","sex","age")) %>% head
#' titanic %>% combinedata(titanic_pred,c("pclass","sex","age")) %>% head
#' avengers %>% combinedata(superheroes, type = "bind_cols")
#' combinedata("avengers", "superheroes", type = "bind_cols")
#' avengers %>% combinedata(superheroes, type = "bind_rows")
#'
#' @export
combinedata <- function(dataset, cmb_dataset,
                        by = "",
                        type = "inner_join",
                        name = "") {

  is_join <- grepl("_join",type)
  if (is_join && by[1] == "")
    return(cat("No variables selected to join datasets\n"))

  if (name == "")
    name <- if (is_string(dataset)) paste0("cmb_",dataset) else "cmb_data"

  if (is_join) {
    dat <- get(type)(getdata(dataset, na.rm = FALSE), getdata(cmb_dataset, na.rm = FALSE), by = by)
    madd <- paste0("\n\nBy: ", paste0(by, collapse = ", "))
  } else {
    dat <- get(type)(getdata(dataset, na.rm = FALSE), getdata(cmb_dataset, na.rm = FALSE))
    madd <- ""
  }

  if (is.character(dat)) return(dat)

  mess <- paste0("\n### Combined\n\nDatasets: ", dataset, " and ",
                        cmb_dataset, " (", type, ")", madd, "\n\nOn: ",
                        lubridate::now())

  if (exists("r_env")) {
    env <- r_env
  } else if (exists("r_data")) {
    env <- pryr::where("r_data")
  } else {
    return(dat)
  }

  env$r_data[[name]] <- dat
  # env$r_data[[name]] %>% head %>% print
  env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique
  env$r_data[[paste0(name,"_descr")]] <- mess
  cat("\nCombined data added as", name, "\n")
}

## something strange going on with bind_cols when there are duplicate names
# if (type == "bind_cols") {
#   if ((intersect(names(getdata(dataset)),names(getdata(cmb_dataset))) %>% length) > 0)
#     return("Error: found duplicated column name")
# }
