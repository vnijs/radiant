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
  pt <- . %>% {if(is.null(.$type)) .$Set(type = "terminal")}
  jl$Do(pt, filterFun = isLeaf)

  ## or use this approach if type not set?
  # set_terminal <- jl$Get("type")
  # if(any(is.na(set_terminal))) {
  #   set_terminal[is.na(set_terminal)] <- "terminal"
  #   jl$Set(type = set_terminal)
  # }

  ## making a copy of the initial Node object
  jl_init <- sshhr(Clone(jl))
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
    x %>% {if(is.na(.)) "" else .} %>%
      format(digits = 10, nsmall = 2, decimal.mark = ".", big.mark = ",", scientific = FALSE)
  }

  ## initial setup
  cat("Initial decision tree:\n")
  object$jl_init %>%
    print(
      Payoff = .$Get("payoff", format = print_money),
      Probability = .$Get("p", format = FormatPercent)
    )

  ## calculations completed
  cat("\n\nFinal decision tree:\n")
  object$jl %>%
    print(
      Payoff = .$Get("payoff", format = print_money),
      Probability = .$Get("p", format = FormatPercent)
    )

  cat("\n\nDecision:\n")
  object$jl$Get("decision") %>% .[!is.na(.)] %>% paste0(collapse = " & ") %>% cat
}

#' Plot method for the dtree function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/dtree.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{dtree}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{summary.single_mean}} to summarize results
#'
#' @export
plot.dtree <- function(x, shiny = FALSE, ...) {

  ## copied from https://github.com/gluc/useR15/blob/master/01_showcase/02_decision_tree.R
  ## fit with diagrammer?

  # library(ape)
  # jl <- x$jl
  # jl$Revert()
  # jlp <- as.phylo(jl)
  # par(mar=c(1,1,1,1))
  # plot(jlp, show.tip.label = FALSE, type = "cladogram")

  # nodelabel <- function(x) {
  #   po <- paste0( '$ ', format(x$payoff, scientific = FALSE, big.mark = "'"))
  #   if (x$type == 'terminal') return (po)
  #   return ( paste0('ER\n', po) )
  # }

  # for (node in jl$leaves) edges(GetPhyloNr(node$parent, "node"), GetPhyloNr(node, "node"), arrows = 2, type = "triangle", angle = 60)

  # for(node in jl$Get(function(x) x)) {
  #   if(node$type == 'decision') {
  #     nodelabels(nodelabel(node), GetPhyloNr(node, "node"), frame = 'none', adj = c(0.3, -0.5))
  #   } else if(node$type == 'chance') {
  #     if (node$name == node$parent$decision) edges(GetPhyloNr(node$parent, "node"), GetPhyloNr(node, "node"), col = "red")
  #     nodelabels(" ", GetPhyloNr(node, "node"), frame = "circle")
  #     nodelabels(nodelabel(node), GetPhyloNr(node, "node"), frame = 'none', adj = c(0.5, -0.5))
  #     edgelabels(node$name, GetPhyloNr(node, "edge"), bg = "white")
  #   } else if(node$type == 'terminal') {
  #     tiplabels(nodelabel(node), GetPhyloNr(node, "node"), frame = "none", adj = c(0.5, -0.6))
  #     edgelabels(paste0(node$name," (", node$p, ")"), GetPhyloNr(node, "edge"), bg = "white")
  #   }
  # }

  # nodelabels("   ", GetPhyloNr(jl, "node"), frame = "rect")

  # sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = min(length(plot_list),2)))) ) %>%
  #   { if (shiny) . else print(.) }
}
