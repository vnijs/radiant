#' Create a decision tree
#'
#' @details See \url{http://vnijs.github.io/radiant/base/dtree.html} for an example in Radiant
#'
#' @param yl A yaml string or a list (e.g., from yaml::yaml.load_file())
#'
#' @return A list with the initial tree and the calculated tree
#'
#' @importFrom yaml yaml.load
#'
#' @seealso \code{\link{summary.dtree}} to summarize results
#' @seealso \code{\link{plot.dtree}} to plot results
#' @export
dtree <- function(yl) {

  ## most of the code in this function is from
  ## https://github.com/gluc/useR15/blob/master/01_showcase/02_decision_tree.R

  ## load yaml from string of list not provide
  if (is_string(yl)) {
    yl <- yaml.load(yl)
    if (is_string(yl)) yl <- getdata(yl) %>% yaml.load(.)
  }
  # if (length(yl) == 0) return("The provided list is empty or not in the correct format")

  ## convert list to node object
  jl <- as.Node(yl)

  ## if type not set and isLeaf set to terminal
  pt <- . %>% {if (is.null(.$type)) .$Set(type = "terminal")}
  jl$Do(pt, filterFun = isLeaf)

  ## or use this approach if type not set?
  # set_terminal <- jl$Get("type")
  # if (any(is.na(set_terminal))) {
  #   set_terminal[is.na(set_terminal)] <- "terminal"
  #   jl$Set(type = set_terminal)
  # }

  ## making a copy of the initial Node object
#   jl_init <- as.Node(yl)
#   pt <- . %>% {if (is.null(.$type)) .$Set(type = "terminal")}
#   jl_init$Do(pt, filterFun = isLeaf)
  ## see issue https://github.com/gluc/data.tree/issues/22
  # jl_init <- sshhr(Clone(jl))
  jl_init <- Clone(jl)
  ## without sshhr:
  ## Warning messages: 1: In res[fieldName] <- field : number of items to replace is not a multiple of replacement length

  ## calculate payoff
  calc_payoff <- function(x) {
    if (x$type == 'chance') x$payoff <- Aggregate(x, function(node) node$payoff * node$p, sum)
    else if (x$type == 'decision') x$payoff <- Aggregate(x, "payoff", max)
  }

  jl$Do(calc_payoff, traversal = "post-order", filterFun = isNotLeaf)

  decision <- function(x) {
    po <- sapply(x$children, function(child) child$payoff)
    x$decision <- names(po[po == x$payoff])
  }

  jl$Do(decision, filterFun = function(x) !is.null(x$type) && x$type == 'decision')

  list(jl_init = jl_init, jl = jl) %>% set_class(c("dtree",class(.)))
}

#' Summary method for the dree function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/dtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{simulater}}
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{dtree}} to generate the results
#' @seealso \code{\link{plot.dtree}} to plot results
#'
#' @export
summary.dtree <- function(object, ...) {

  print_money <- function(x) {
    x %>% {if (is.na(.)) "" else .} %>%
      format(digits = 10, nsmall = 2, decimal.mark = ".", big.mark = ",", scientific = FALSE)
  }

  rm_terminal <- function(x)
    x %>% {if (is.na(.)) "" else .} %>% {if (. == "terminal") "" else .}

  # format_percent <- . %>% as.character %>% sprintf("%.2f%%", . * 100)

  ## format data.tree
  format_dtree <- function(jl) {
    ## set parent type
    nt <- jl$Get(function(x) x$parent$type)
    jl$Set(ptype = nt)

    Traverse(jl) %>%
      {data.frame(Probability = Get(., "p", format = FormatPercent),
        Payoff = Get(., "payoff", format = print_money),
        Type = Get(., "ptype", format = rm_terminal),
        check.names = FALSE,
        row.names = Get(.,"levelName"))}
  }

  ## initial setup
  cat("Initial decision tree:\n")
  format_dtree(object$jl_init) %>% print

  cat("\n\nFinal decision tree:\n")
  format_dtree(object$jl) %>% print

  cat("\n\nDecision:\n")
  object$jl$Get("decision") %>% .[!is.na(.)] %>% paste0(collapse = " & ") %>% cat

  ## useful to avoid row.names and left-align all character variables
  # format(justify = "left") %>%
  # print(row.names = FALSE)
}

#' Plot method for the dtree function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/dtree.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{dtree}}
#' @param final If TRUE plot the decision tree solution, else the initial decision tree
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{dtree}} to generate the result
#' @seealso \code{\link{summary.dtree}} to summarize results
#'
#' @export
plot.dtree <- function(x, final = FALSE, shiny = FALSE, ...) {

  ## based on https://gist.github.com/gluc/79ef7a0e747f217ca45e
  jl <- if (final) x$jl else x$jl_init

  ## create ids
  jl$Set(id = paste0("id", 1:jl$totalCount))

  ## create start labels
  FromLabel <- function(node) {
    if (node$parent$isRoot) ToLabel(node$parent)
    else as.character(node$parent$id)
  }

  ## create arrow labels
  EdgeLabel <- function(node) {
    if (node$isRoot) {
      return (" ")
    } else if (node$parent$type == "decision") {
      lbl <- node$name
    } else if (node$parent$type == "chance") {
      lbl <- paste0(node$name,": ", node$p)
    } else if (node$type == "terminal") {
      lbl <- paste0(node$name,": ", node$p)
    }

    if (!is.null(node$parent$decision) && node$name == node$parent$decision)
      paste0(" === |", lbl, "|")
    else
      paste0(" --- |", lbl, "|")
  }

  FormatPayoff <- function(payoff) {
    paste0("$", format(payoff, scientific = FALSE, big.mark = ","))
  }

  ToLabel <- function(node) {
    po <- if (final) FormatPayoff(node$payoff) else " "
    if (node$type == "decision") {
      lbl <- paste0("[", po, "]")
    } else if (node$type == "chance") {
      lbl <- paste0("((", po, "))")
    } else if (node$type == "terminal") {
      lbl <- paste0("[", FormatPayoff(node$payoff), "]")
    }
    paste0(" ", node$id, lbl)
  }

  style <- paste0(
    "classDef default fill:none, bg:none, stroke-width:0px;
    classDef chance fill:#FF8C00,stroke:#333,stroke-width:1px;
    classDef decision fill:#9ACD32,stroke:#333,stroke-width:1px;
    class ", paste(jl$Get("id", filterFun = function(x) x$type == "decision"), collapse = ","), " decision;
    class ", paste(jl$Get("id", filterFun = function(x) x$type == "chance"), collapse = ","), " chance;")
  trv <- Traverse(jl, traversal = "level", filterFun = isNotRoot)
  df <- data.frame(from = Get(trv, FromLabel), edge = Get(trv, EdgeLabel), to = Get(trv, ToLabel))

  paste("graph LR", paste( paste0(df$from,df$edge, df$to), collapse = "\n"),
    style, sep = "\n") %>%
    {if (shiny) . else DiagrammeR::DiagrammeR(.)}
}

# library(data.tree); library(yaml); library(radiant)
#
# yl <- yaml::yaml.load_file("~/Dropbox/teaching/MGT403-2015/data.tree/jennylind.yaml")
# object <- x <- dtree(yl)
# print(object$jl)

# df <- ToDataFrameTree(object$jl, "p", "payoff")
# df <- ToDataFrameTable(object$jl, "p", "payoff")
# df <- ToDataFrameTaxonomy(object$jl, "p", "payoff")
# print(df)
#
# ps <- .4
# object$jl$Set(ps, filterFun = function(x) x$name == "Small Box Office")
#
# pf <- function(self) 1 - self$parent$SmallBoxOffice$ps
# object$jl$Set(pf, filterFun = function(x) x$name == "Medium Box Office")
#
# ps <- c(.5, .3, .2)
# object$jl$Set(ps, filterFun = function(x) x$name %in% c("Small Box Office","Medium Box Office", "Large Box Office"))
#
# df <- ToDataFrameTaxonomy(object$jl, "p", "payoff", "ps")
# print(df)
#
# ps <- 0
# object$jl$Set(ps, filterFun = function(x) x$type == "chance")
#
# prob_prob <- c()
# check_prob <- function(x) {
#   if (x$type == 'chance') x$prob <- Aggregate(x, function(node) node$p, sum)
#   if(!is.null(x$prob) && x$prob == 1) prob_prob <<- c(prob_prob, x$name)
#   # if(!is.null(x$prob) && x$prob == 1) prob_prob <<- x$name
# }
#
# object$jl$Do(check_prob, traversal = "post-order", filterFun = isNotLeaf)
# prob_prob
#
# df <- ToDataFrameTaxonomy(object$jl, "p", "payoff", "ps","prob")
# print(df)
#
#
# decision <- function(x) {
#   po <- sapply(x$children, function(child) child$payoff)
#   x$decision <- names(po[po == x$payoff])
# }
#
#   jl$Do(decision, filterFun = function(x) !is.null(x$type) && x$type == 'decision')


#
# print(df)
#
#
# df <- ToDataFrameTaxonomy(object$jl, "p", "ps", "payoff")
# print(df)



# df[df$children == "Small Box Office","p"] <- .8
# df[df$children == "Medium Box Office","p"] <-
#   1 - df[df$children == "Small Box Office","p"]  - df[df$children == "Large Box Office","p"]

# `Small Box Office` = c(.1, .2, .3)
# `Median Box Office` = 1 - `Small Box Office`

# expand.grid

# as.Node(df)


# x %>% summary
# dtree(yl) %>% plot(shiny = TRUE)
# dtree(yl) %>% plot(final = TRUE)
# dtree(yl) %>% plot(final = FALSE)
# yl <- yaml::yaml.load_file("~/Dropbox/teaching/MGT403-2015/data.tree/quant_job.yaml")
# dtree(yl) %>% summary
# dtree(yl) %>% plot
# dtree(yl) %>% plot(final = TRUE)

# shiny::runApp("~/gh/radiant/inst/quant")
