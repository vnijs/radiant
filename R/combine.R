#' Combine datasets using dplyr's bind and join functions
#'
#' @details See \url{http://vnijs.github.io/radiant/base/combine.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param dataset2 Dataset name (string) to combine with `dataset`. This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param cmb_vars Variables used to combine `dataset` and `dataset2`
#' @param cmb_type The main bind and join types from the dplyr package are provided. `inner_join` returns all rows from x with matching values in y, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. `left_join` returns all rows from x, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. `right_join` is equivalent to a `left_join` for datasets y and x. `full_join` combines two datasets, keeping rows and columns that appear in either. `semi_join` returns all rows from x with matching values in y, keeping just columns from x. A semi join differs from an inner join because an inner join will return one row of x for each matching row of y, whereas a semi join will never duplicate rows of x. `anti_join` returns all rows from x without matching values in y, keeping only columns from x.
#' @param cmb_name Name for the combined dataset
#'
#' @return If list `r_data` exists the combined dataset is added as `cmb_name`. Else the combined dataset will be returned as `cmb_name`
#'
#' @examples
#' combinedata("titanic","titanic_pred",c("pclass","sex","age")) %>% head
#' titanic %>% combinedata("titanic_pred",c("pclass","sex","age")) %>% head
#' titanic %>% combinedata(titanic_pred,c("pclass","sex","age")) %>% head
#'
#' @export
combinedata <- function(dataset, dataset2,
                      cmb_vars = "",
                      cmb_type = "bind_rows",
                      cmb_name = "cmb_data") {

  is_join <- grepl("_join",cmb_type)
  if (is_join && cmb_vars[1] == "")
    return(cat("No variables selected to join datasets"))

  if(is_string(dataset)) cmb_name = paste0("cmb_",dataset)

  # cmb_fun <- get(cmb_type)
  if(is_join) {
    cmb_dat <- get(cmb_type)(getdata(dataset), getdata(dataset2), by = cmb_vars)
    cmb_message <- paste0("\n### Combined\n\nDatasets: ", dataset, " and ", dataset2, " (", cmb_type, ")\n\nBy: ", paste0(cmb_vars, collapse=", "), "\n\nOn: ", now())
  } else {
    cmb_dat <- get(cmb_type)(getdata(dataset), getdata(dataset2))
    cmb_message <- paste0("\n### Combined\n\nDatasets: ", dataset, " and ", dataset2, " (", cmb_type, ")\n\nOn: ", now())
  }

  if (exists("r_env")) {
    c_env <- r_env
  } else if (exists("r_data")) {
    c_env <- pryr::where("r_data")
    # d_env$r_data[[cmb_name]] <- cmb_fun(getdata(dataset), getdata(dataset2), by = cmb_vars)
    # d_env$r_data[[cmb_name]] %>% head %>% print
    # d_env$r_data[['datasetlist']] <- c(cmb_name, d_env$r_data[['datasetlist']]) %>% unique
    # d_env$r_data[[paste0(cmb_name,"_descr")]] <-
    #   paste0("\n### Merged\n\nDatasets: ", dataset, " and ", dataset2, " (", cmb_type, ")\n\nBy: ", paste0(cmb_vars, collapse=", "), "\n\nOn: ", now())
    # cat("\nMerged data added to r_data as", cmb_name, "\n")
  } else {
    # return(cmb_fun(getdata(dataset), getdata(dataset2), by = cmb_vars))
    return(cmb_dat)
  }

  # c_env$r_data[[cmb_name]] <- cmb_fun(getdata(dataset), getdata(dataset2), by = cmb_vars)
  c_env$r_data[[cmb_name]] <- cmb_dat
  c_env$r_data[[cmb_name]] %>% head %>% print
  c_env$r_data[['datasetlist']] <- c(cmb_name, c_env$r_data[['datasetlist']]) %>% unique
  c_env$r_data[[paste0(cmb_name,"_descr")]] <- cmb_message
  cat("\nCombined data added as", cmb_name, "\n")
}
