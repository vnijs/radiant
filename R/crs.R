#' Collaborative Filtering
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/crs.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param id String with name of the variable containing user ids
#' @param prod String with name of the variable with product ids
#' @param pred Products to predict for
#' @param rate String with name of the variable with product ratings
#' @param name Name for the prediction variable
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A data.frame with the original data and a new column with predicted ratings
#'
#' @export
crs <- function(dataset, id, prod, pred, rate, name = "pred", data_filter = "") {

  if (data_filter == "")
    return("A data filter must be set" %>% set_class(c("crs",class(.))))

  # library(radiant)
  # loadr("~/ca/week7/crs/cf_demo.rda")
  # dataset <- "cf_demo"
  # id <- "Users"
  # prod <- "Movies"
  # rate <- "Ratings"
  # data_filter <- "training == 1"
  # pred

  vars <- c(id, prod, rate)
  dat <- getdata(dataset, vars, na.rm = FALSE)
  if (!is_string(dataset)) dataset <- "-----"

  ## creating a matrix layout
  ## NOT efficient -- improvement possible with dplyr or sparse matrix?
  dat <- spread_(dat, prod, rate)

  idv <- select_(dat,id)
  uid <- getdata(dataset, id, filt = data_filter, na.rm = FALSE) %>% unique
  uid <- seq_len(nrow(dat))[idv[[1]] %in% uid[[1]]]

  dat <- select_(dat, paste0("-",id))

  ## stop if insufficient overlap in ratings
  if (length(pred) >= (ncol(dat) - 1))
    return("Cannot predict for all products. Ratings must overlap on at least two products." %>% set_class(c("crs",class(.))))

  ## indices
  cn <- colnames(dat)
  nind <- which(cn %in% pred)
  ind <- (1:length(cn))[-nind]

  ## average scores and rankings
  avg <- slice(dat, uid) %>% select(nind) %>% summarize_each(funs(mean_rm))
  ravg <- min_rank(desc(avg)) %>% t %>% as.data.frame
  names(ravg) <- names(avg)

  ## actual scores and rankings (if available, else will be NA)
  act <- slice(dat, -uid) %>% select(nind)

  ## ract line below doesn't work with ratings0 when only one movie has been selected
  # print(act)
  # return()

  ract <- as.data.frame(t(apply(act,1, function(x) min_rank(desc(x))))) %>%
    bind_cols(slice(idv,-uid),.) %>% as_data_frame
  act <- bind_cols(slice(idv,-uid),act) %>% as.data.frame

  ## CF calculations
  ms <- apply(select(dat,-nind), 1, function(x) mean(x, na.rm = TRUE))
  sds <- apply(select(dat,-nind), 1, function(x) sd(x, na.rm = TRUE))

  # print(head(dat))
  # print(print(environment() %>% as.list))
  # print(sys.call())
  # return(environment() %>% as.list %>% set_class(c("crs",class(.))))

  ## to forego standardization
  # ms <- ms * 0
  # sds <- sds/sds

  if (length(nind) < 2) {
    srate <- (dat[uid,nind] - ms[uid]) / sds[uid]
  } else {
    srate <- sweep(dat[uid,nind], 1, ms[uid], "-") %>% sweep(1, sds[uid] ,"/")
  }

  cors <- cor(t(dat[uid, ind]), t(dat[-uid, ind]), use = "pairwise.complete.obs")
  dnom <- apply(cors, 2, function(x) sum(abs(x)))
  wts <- sweep(cors, 2, dnom, "/")
  cf <-
    (crossprod(wts, as.matrix(srate)) * sds[-uid] + ms[-uid]) %>%
    as.data.frame %>%
    bind_cols(slice(idv,-uid),.) %>%
    as.data.frame

  if (ncol(cf) == 2) colnames(cf) <- c(id, pred)

  ## Ranking based on CF
  rcf <- as.data.frame(t(apply(select(cf,-1),1, function(x) min_rank(desc(x))))) %>%
    bind_cols(slice(idv,-uid),.) %>% as.data.frame

  rm(dat, ms, sds, srate, cors, dnom, wts, cn, ind, nind)

  environment() %>% as.list %>% set_class(c("crs",class(.)))
}

#' Summary method for Collaborative Filter
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/crs.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{simulater}}
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{crs}} to generate the results
#' @seealso \code{\link{plot.crs}} to plot results
#'
#' @export
summary.crs <- function(object, ...) {

  if (is.character(object))
    return(cat(object))

  # return("here")

  cat("Avergage Ratings:\n\n")
  print(dfprint(object$avg, dec = 2), row.names = FALSE)

  cat("\nAvergage Rankings:\n\n")
  print(dfprint(object$ravg, dec = 2), row.names = FALSE)

  cat("\nRatings from Collaborative Filter:\n\n")
  print(dfprint(object$cf, dec = 2), row.names = FALSE)

  cat("\nRankings from Collaborative Filter:\n\n")
  print(dfprint(object$rcf, dec = 2), row.names = FALSE)

  if (!any(is.na(object$act))) {
    cat("\nActual ratings:\n\n")
    print(dfprint(object$act, dec = 2), row.names = FALSE)

    cat("\nActual rankings:\n\n")
    print(dfprint(object$ract, dec = 2), row.names = FALSE)
  }
}

#' Plot method for the crs function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/crs.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{crs}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{crs}} to generate results
#' @seealso \code{\link{summary.crs}} to summarize results
#'
#' @export
plot.crs <- function(x, shiny = FALSE, ...) {

  # object <- x; rm(x)
  # if (is.character(object)) return(object)

  return("Plotting for Collaborative Filter not yet available")

  # object <- crs(dataset, id, prod, pred, rate, name = "pred", data_filter = data_filter)

  # dat <- gather_(object$cf, "product", "cf", colnames(object$cf)[-1])
  # rcf <- gather_(object$rcf, "product", "rcf", colnames(object$rcf)[-1])

  # act <- gather_(object$act, "product", "act", colnames(object$act)[-1])
  # ract <- gather_(object$ract, "product", "ract", colnames(object$ract)[-1])

  # dat <- bind_cols(dat, select_(rcf, "rcf"), select_(act, "act"), select_(ract, "ract")) %>% arrange(user, product)

  # avg <- object$avg[1,] %>% unlist %>% unname
  # ravg <- object$ravg[1,] %>% unlist %>% unname
  # dat <- cbind(dat, avg, ravg) %>% arrange(user, product)

  # dat <- select(dat, user, product, act, avg, cf, ract, ravg, rcf) %>%
  #   arrange(user, product)
  # # dat %>% filter(user %in% 62:63) %>%
  # # write.csv(file = "~/ca/week7/cf-table.csv", row.names = FALSE)

  # visualize(dat, xvar = "cf", yvar = "act", type = "scatter", facet_col = "product", check = "line", data_filter = "", custom = TRUE) +
  # geom_segment(aes(x = 1, y = 1, xend = 5, yend = 5), color = "blue", size = .05) +
  # ylim(1,5) + xlim(1,5) +
  # xlab("Predicted ratings") +
  # ylab("Actual ratings") +
  # ggtitle("Predictions from Collaborative Filtering") +
  # theme(legend.position = "none")

  # sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
  #   { if (shiny) . else print(.) }

}

#' Store predicted values generated in the crs function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/crs.html} for an example in Radiant
#'
#' @param pred Return value from predict.nnet
#' @param data Dataset name
#' @param name Variable name assigned to the predicted values
#'
#' @export
store_crs <- function(pred, data,
                      name = "pred_crs") {

  ## merging data
  # combinedata(dataset, cf, by = id, add = "pred", type = "left_join", name = dataset)

  ## fix empty name input
  # if (gsub("\\s","",name) == "") name <- "pred_ann"
  # changedata(data, vars = pred, var_names = name)
}
