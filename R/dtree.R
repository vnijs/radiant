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

  rm_terminal <- . %>% {if(. == "terminal") "" else .}

  ## initial setup
  cat("Initial decision tree:\n")
  object$jl_init %>%
    print(
      # Level = .$Get("levelName"),
      Probability = .$Get("p", format = FormatPercent),
      Payoff = .$Get("payoff", format = print_money),
      Type = .$Get("type", format = rm_terminal)
    )

  ## calculations completed
  cat("\n\nFinal decision tree:\n")
  object$jl %>%
    print(
      Probability = .$Get("p", format = FormatPercent),
      Payoff = .$Get("payoff", format = print_money),
      Type = .$Get("type", format = rm_terminal)
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

  library(DiagrammeR)
  library(ape)
  jl <- x$jl

  ## reverse sort order
  # jl$Revert()

  ## needed for ape
  # jlp <- as.phylo(jl)
  # par(mar=c(1,1,1,1))
  # plot(jlp, show.tip.label = FALSE, type = "cladogram")

  nodelabel <- function(x) {
    paste0( '$ ', format(x$payoff, scientific = FALSE, big.mark = ","))
  }

  ## all leaves
  # for (node in jl$leaves) print(node$parent)

#   jl$levelName
#   jl$isRoot
#   jl$children

  mm <- "graph LR\n"

  id <- 0
  for(node in jl$Get(function(x) x)) {
    id <- id + 1

    mm %<>% paste0("id",id," ",node$name, " ", node$type, "\n")
#     if(node$isRoot)  mm %<>% paste0("id",id,"[ ] --> \n")
#
#     if(node$type == 'decision') {
#       mm %<>% paste0("id",id," ",node$name, " ", node$type, "\n")
#     } else if(node$type == 'chance') {
#       mm %<>% paste0("id",id," ",node$name, " ", node$type, "\n")
#     } else if(node$type == 'terminal') {
#       mm %<>% paste0("id",id," ",node$name, " ", node$type, "\n")
#     }
  }
  cat(mm)

  # desired end product - replace A, B, etc. by id1, id2, etc.
  mm %<>% paste0("A[ ] --> |Accept John's Offer| B[$12,000]\n A --> |Reject John's Offer |C(( ))\nC --> |Offer from Vanessa 0.6| D[ ]\n D --> |Accept Vanessa's Offer| E[$14,000]\n D --> |Reject Vanessa's Offer| F(( ))\n C --> |No Offer from Vanessa 0.4| G(( ))\n G --> |Salary 1 0.05| H[$21,600]\n G --> |Salary 2 0.25| I[$16,800]\n G --> |Salary 3 0.40| J[$12,800]\n G --> |Salary 4 0.25| K[$6,000]\n G --> |Salary 5 0.05| L[$0]\n \n F --> |Salary 1 0.05| M[$21,600]\n F --> |Salary 2 0.25| N[$16,800]\n F --> |Salary 3 0.40| O[$12,800]\n F --> |Salary 4 0.25| P[$6,000]\n F --> |Salary 5 0.05| Q[$0]\n \n class A,D decision;\n class C,G,F chance;\n")

  ## formatting specifics for chance and decision nodes
  mm %<>% paste0("\nclassDef default fill:none, bg:none, stroke-width:0px;\nclassDef decision fill:#9f6,stroke:#333,stroke-width:1px;\nclassDef chance fill:red,stroke:#333,stroke-width:1px;")

  ## produces the plot
  mermaid(mm)

  ## code for ape
#   for(node in jl$Get(function(x) x)) {
#     if(node$type == 'decision') {
#       nodelabels(" ", GetPhyloNr(node, "node"), bg = "green")
#       nodelabels(nodelabel(node), GetPhyloNr(node, "node"), frame = "none", adj = c(0.3, -0.5))
#     } else if(node$type == 'chance') {
#       if (!is.null(node$parent$decision) && node$name == node$parent$decision) edges(GetPhyloNr(node$parent, "node"), GetPhyloNr(node, "node"), col = "red")
#       nodelabels(" ", GetPhyloNr(node, "node"), frame = "circle", bg = "red")
#       nodelabels(nodelabel(node), GetPhyloNr(node, "node"), frame = 'none', adj = c(0.5, -0.5))
#       if (!is.null(node$p)) edgelabels(paste0(node$name," (", node$p, ")"), GetPhyloNr(node, "edge"), bg = "none", frame = 'none')
#       else edgelabels(node$name, GetPhyloNr(node, "edge"), bg = "none", frame = 'none')
#     } else if(node$type == 'terminal') {
#       tiplabels(nodelabel(node), GetPhyloNr(node, "node"), frame = "none", adj = c(0.5, -0.6))
#       if (!is.null(node$p)) edgelabels(paste0(node$name," (", node$p, ")"), GetPhyloNr(node, "edge"), bg = "none", frame = 'none')
#       else edgelabels(node$name, GetPhyloNr(node, "edge"), bg = "none", frame = 'none')
#     }
#   }

  # nodelabels("   ", GetPhyloNr(jl, "node"), frame = "none")

  # sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = min(length(plot_list),2)))) ) %>%
  #   { if (shiny) . else print(.) }
}

# library(data.tree); library(yaml); library(radiant)
# yl <- yaml::yaml.load_file("~/Dropbox/teaching/MGT403-2015/data.tree/quant_job.yaml")
# x <- dtree(yl)
# dtree(yl) %>% summary
# yl <- dtree(yl)
# save(yl, file = "~/Dropbox/teaching/MGT403-2015/data.tree/quant_job.rda")

# mermaid("
# graph LR
#
#         D --> |Reject Vanessa's Offer| F(( ))
#         C --> |No Offer from Vanessa 0.4| G(( ))
#         G --> |Salary 1 0.05| H[$21,600]
#         G --> |Salary 2 0.25| I[$16,800]
#         G --> |Salary 3 0.40| J[$12,800]
#         G --> |Salary 4 0.25| K[$6,000]
#         G --> |Salary 5 0.05| L[$0]
#
#         F --> |Salary 1 0.05| M[$21,600]
#         F --> |Salary 2 0.25| N[$16,800]
#         F --> |Salary 3 0.40| O[$12,800]
#         F --> |Salary 4 0.25| P[$6,000]
#         F --> |Salary 5 0.05| Q[$0]
#         A[ ] --> |Accept John's Offer| B[$12,000]
#         A --> |Reject John's Offer |C(( ))
#         C --> |Offer from Vanessa 0.6| D[ ]
#         D --> |Accept Vanessa's Offer| E[$14,000]
#
#         classDef default fill:none, bg:none, stroke-width:0px;
#         classDef decision fill:#9f6,stroke:#333,stroke-width:1px;
#         classDef chance fill:red,stroke:#333,stroke-width:1px;
#         class A,D decision;
#         class C,G,F chance;
# ")
