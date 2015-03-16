#' Merge datasets using dplyr's join functions
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/base/merge.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param dataset2 Dataset name (string) to merge with `dataset`. This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param merge_vars Variables used to merge/join `dataset` and `dataset2`
#' @param merge_type The main join types from the dplyr package are provided. `inner_join` returns all rows from x with matching values in y, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. `left_join` returns all rows from x, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. `semi_join` returns all rows from x with matching values in y, keeping just columns from x. A semi join differs from an inner join because an inner join will return one row of x for each matching row of y, whereas a semi join will never duplicate rows of x. `anti_join` returns all rows from x without matching values in y, keeping only columns from x
#' @param merge_name Name for the merged dataset
#'
#' @return If (reactive) list `r_data` exists the merged dataset added as `merge_name`. Else the merged dataset will be returned as `merge_name`
#'
#' @examples
#' mergedata("titanic","titanic_pred",c("pclass","sex","age"))
#'
#' @export
mergedata <- function(dataset, dataset2,
                       merge_vars = "",
                       merge_type = "inner_join",
                       merge_name = paste0("merged_",dataset)) {

  if(merge_vars[1] == "") return(cat("No variables selected to merge datasets"))
  merge_fun <- get(merge_type)

  if(exists("r_env")) {
    r_env$r_data[[merge_name]] <- merge_fun(getdata(dataset), getdata(dataset2), by = merge_vars)
    r_env$r_data[[merge_name]] %>% head %>% print
    r_env$r_data[['datasetlist']] <- c(merge_name,r_data[['datasetlist']]) %>% unique
    r_env$r_data[[paste0(merge_name,"_descr")]] <-
      paste0("\n### Merged\n\nDatasets: ", dataset, " and ", dataset2, "\n\nBy: ", paste0(merge_vars, collapse=", "), "\n\nOn: ", now())
    cat("\nMerged data added as", merge_name)
  }
  # else if(exists("r_data")) {
  #   r_data <- get("r_data"); r_data[[merge_name]] <- NULL
  #   r_data[[merge_name]] <<- merge_fun(getdata(dataset), getdata(dataset2), by = merge_vars)
  #   r_data[[merge_name]] %>% head %>% print
  #   r_data[['datasetlist']] <<- c(merge_name,r_data[['datasetlist']]) %>% unique
  #   r_data[[paste0(merge_name,"_descr")]] <<-
  #     paste0("\n### Merged\n\nDatasets: ", dataset, " and ", dataset2, "\n\nBy: ", paste0(merge_vars, collapse=", "), "\n\nOn: ", now())
  #   cat("\nMerged data added to r_data as", merge_name)
  # }
  else {
    merge_fun(getdata(dataset), getdata(dataset2), by = merge_vars)
  }
}
