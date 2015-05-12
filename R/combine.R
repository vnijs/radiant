#' Combine datasets using dplyr's bind and join functions
#'
#' @details See \url{http://vnijs.github.io/radiant/base/combine.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param dataset2 Dataset name (string) to combine with `dataset`. This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param cmb_vars Variables used to combine `dataset` and `dataset2`
#' @param cmb_type The main bind and join types from the dplyr package are provided. \bold{inner_join} returns all rows from x with matching values in y, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. \bold{left_join} returns all rows from x, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. \bold{right_join} is equivalent to a left join for datasets y and x. \bold{full_join} combines two datasets, keeping rows and columns that appear in either. \bold{semi_join} returns all rows from x with matching values in y, keeping just columns from x. A semi join differs from an inner join because an inner join will return one row of x for each matching row of y, whereas a semi join will never duplicate rows of x. \bold{anti_join} returns all rows from x without matching values in y, keeping only columns from x. \bold{bind_rows} and \bold{bind_cols} are also included, as are \bold{intersect}, \bold{union}, and \bold{setdiff}. See \url{http://vnijs.github.io/radiant/base/combine.html} for further details
#' @param cmb_name Name for the combined dataset
#'
#' @return If list `r_data` exists the combined dataset is added as `cmb_name`. Else the combined dataset will be returned as `cmb_name`
#'
#' @examples
#' combinedata("titanic","titanic_pred",c("pclass","sex","age")) %>% head
#' titanic %>% combinedata("titanic_pred",c("pclass","sex","age")) %>% head
#' titanic %>% combinedata(titanic_pred,c("pclass","sex","age")) %>% head
#' avengers %>% combinedata(superheroes, cmb_type = "bind_cols")
#' combinedata("avengers", "superheroes", cmb_type = "bind_cols")
#' avengers %>% combinedata(superheroes, cmb_type = "bind_rows")
#'
#' @export
combinedata <- function(dataset, dataset2,
                        cmb_vars = "",
                        cmb_type = "inner_join",
                        cmb_name = "") {

  is_join <- grepl("_join",cmb_type)
  if (is_join && cmb_vars[1] == "")
    return(cat("No variables selected to join datasets\n"))

  if (cmb_name == "")
    cmb_name <- if (is_string(dataset)) paste0("cmb_",dataset) else "cmb_data"

#   if (cmb_type == "bind_cols") {
#     if((intersect(names(getdata(dataset)),names(getdata(dataset2))) %>% length) > 0)
#       return("Error: found duplicated column name")
#   }

  if (is_join) {
    cmb_dat <- get(cmb_type)(getdata(dataset), getdata(dataset2), by = cmb_vars)
    cmb_madd <- paste0("\n\nBy: ", paste0(cmb_vars, collapse = ", "))
  } else {
    cmb_dat <- get(cmb_type)(getdata(dataset), getdata(dataset2))
    cmb_madd <- ""
  }

  if (is.character(cmb_dat)) return(cmb_dat)

  cmb_message <- paste0("\n### Combined\n\nDatasets: ", dataset, " and ",
                        dataset2, " (", cmb_type, ")", cmb_madd, "\n\nOn: ",
                        lubridate::now())

  if (exists("r_env")) {
    c_env <- r_env
  } else if (exists("r_data")) {
    c_env <- pryr::where("r_data")
  } else {
    return(cmb_dat)
  }

  c_env$r_data[[cmb_name]] <- cmb_dat
  c_env$r_data[[cmb_name]] %>% head %>% print
  c_env$r_data[['datasetlist']] <- c(cmb_name, c_env$r_data[['datasetlist']]) %>% unique
  c_env$r_data[[paste0(cmb_name,"_descr")]] <- cmb_message
  cat("\nCombined data added as", cmb_name, "\n")
}
