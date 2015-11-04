
# visNetwork looks fantastic!!
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110
# https://github.com/rich-iannone/DiagrammeR/issues/110

#' Parse yaml input for dtree to provide (more) useful error messages
#'
#' @details See \url{http://vnijs.github.io/radiant/base/dtree.html} for an example in Radiant
#'
#' @param yl A yaml string
#'
#' @return An updated yaml string or a vector messages to return to the users
#'
#' @seealso \code{\link{dtree}} to calculate tree
#' @seealso \code{\link{summary.dtree}} to summarize results
#' @seealso \code{\link{plot.dtree}} to plot results
#'
#' @export
dtree_parser <- function(yl) {
  ############################
  ## test
  # library(radiant)
  # yl <- readLines("~/Dropbox/teaching/MGT403-2015/homework/radiant/chapter1/bio-imaging/bio-imaging-input.yaml")
  # yl <- readLines("~/Dropbox/teaching/MGT403-2015/homework/radiant/chapter1/tee-times/tee-times-monday-input.yaml")
  # yl <- paste0(yl, collapse = "\n")
  ############################

  if (is_string(yl)) yl <- unlist(strsplit(yl, "\n"))

  ## substitute values
  var_def <- grepl("=",yl) %>% which
  if (length(var_def) > 0) {
    for (i in var_def) {
      var <- strsplit(yl[i], "=")[[1]]
      var[1] <- gsub("^\\s+|\\s+$", "", var[1]) %>% gsub("(\\W)", "\\\\\\1", .)
      var[2] <- eval(parse(text = gsub("[a-zA-Z]+","",var[2])))
      # yl[-i] <- gsub(paste0(":(\\s*)",var[1],"\\s*$"), paste0(":\\1 ",var[2]), yl[-i], perl = TRUE)
      yl[-i] <- gsub(paste0(":\\s*",var[1],"\\s*$"), paste0(": ",var[2]), yl[-i], perl = TRUE)

      # var <- c("MF", .15)
      # var <- c("MF", .15)
      # gsub(paste0(":(\\s*)",var[1],"\\s*$"), paste0(":\\1 ",var[2]), ": NMF\n :MF", perl = TRUE)

      # yl[-i] <- gsub(paste0(":\\s*",var[1]), paste0(": ",var[2]), yl[-i], fixed = TRUE)
      ## can't work in the variable definition section
      # yl[-i] <- gsub(paste0(":\\s*",var[1]), paste0(": ",var[2]), yl[-i])
    }
    yl[var_def] <- paste0("# ", yl[var_def])
  }

  ## collect errors
  err <- c()

  ## cheching if a : is present
  # yl <- c(yl, "something without a colon")
  col_ln <- yl %>% grepl("(?=:)|(?=^\\s*$)|(?=^\\s*#)",., perl = TRUE)
  if (any(!col_ln))
    err <- c(err, paste0("Each line must have a ':'. Add a ':' in line(s): ", paste0(which(!col_ln), collapse = ", ")))

  ## add a space to input after the : YAML needs this
  yl %<>% gsub(":([^\\s$])",": \\1", .) %>% gsub("  ", " ", .)

  ## replace .4 by 0.4
  # yl <- c(yl, "p: .4")
  yl %<>% gsub("(^\\s*p\\s*:)\\s*(\\.[0-9]+$)","\\1 0\\2", .,  perl = TRUE)

  ## make sure the labels are in lower case
  yl %<>% gsub("(^\\s*)name(\\s*:)","\\1name\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)type(\\s*:)","\\1type\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)p(\\s*:)","\\1p\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)payoff(\\s*:)","\\1payoff\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)cost(\\s*:)","\\1cost\\2", ., ignore.case = TRUE, perl = TRUE)

  ## check type line is followed by a name
  # yl <- c(yl, "   type   : another   ")
  type_id <- yl %>% grepl("^\\s*type\\s*:\\s*(.*)$",., perl = TRUE) %>% which
  type_cid <- yl %>% grepl("^\\s*type\\s*:\\s*((chance)|(decision)|())\\s*$",., perl = TRUE) %>% which

  if (!identical(type_id, type_cid))
    err <- c(err, paste0("Node type should be 'type: chance', or 'type: decision' in line(s): ", paste0(setdiff(type_id, type_cid), collapse = ", ")))

  ## can't have # signs anywhere if line is not a comment
  # yl <- c(" # name # 3:")
  # yl <- c(" storm leaves # 4 now:")
  # yl <- c(" storm # leaves # 4 now:")
  # yl %<>% gsub("(^\\s*[^\\s\\#]+\\s*)(\\#)", "\\1//", .,  perl = TRUE)
  # yl %<>% gsub("(^\\s*[^#][^#]+\\s*)#", "\\1//", .,  perl = TRUE)
  ## incase there are 2 # signs - should be able to do that in
  # yl %<>% gsub("(^\\s*[^#][^#]+\\s*)#", "\\1//", .,  perl = TRUE)
  nc_id <- yl %>% grepl("^\\s*#", .,  perl = TRUE) %>% {. == FALSE} %>% which

  if (length(nc_id) > 0) {
    yl[nc_id] %<>% gsub("#", "//", .,  perl = TRUE) %>%
      gsub("(^\\s*)[\\!`@%&\\*-\\+]*\\s*", "\\1", .,  perl = TRUE)
  }

  ## Find node names
  # yl <- c(" # name 3:")
  # yl <- c(" p:   ", "  type: ")
  # yl <- c(" name 3:")
  # nn_id <- yl %>% grepl("^[^:#]+:\\s*$",., perl = TRUE) %>% which
  # nn_id <- yl %>% grepl("^\\s*[^#]+[^:]+:\\s*$",., perl = TRUE) %>% which
  nn_id <-
    yl %>% gsub("(^\\s*p\\s*:\\s*$)","\\1 0",.) %>%
      gsub("(^\\s*type\\s*:\\s*$)","\\1 0",.) %>%
      gsub("(^\\s*cost\\s*:\\s*$)","\\1 0",.) %>%
      gsub("(^\\s*payoff\\s*:\\s*$)","\\1 0",.) %>%
      grepl("^\\s*[^#]+:\\s*$",., perl = TRUE) %>% which

  ## replace ( ) { } [ ]
  if (length(nn_id) > 0)
    yl[nn_id] %<>% gsub("[\\(\\)\\{\\}\\[\\]<>\\@;~]", "/", .,  perl = TRUE)

  ## check that type is followed by a name
  # yl <- c(yl, "   type   :  decision   ")

  ## non-commented next line after type
  ncnl_id <- c()
  for (i in type_cid) {
    ncnl_id <- c(ncnl_id, nc_id[nc_id > i][1])
  }

  # type_nn <- type_cid %in% (nn_id - 1)
  type_nn <- ncnl_id %in% nn_id

  if (!all(type_nn))
    err <- c(err, paste0("The node types defined on line(s) ", paste0(type_cid[!type_nn], collapse = ", "), " must be followed by a node name.\nA valid node name could be 'mud slide:'"))

  ## check indent of next line is the same for type defs
  indent_type <- yl[type_cid] %>% gsub("^(\\s*).*","\\1", .) %>% nchar
  # indent_next <- yl[type_cid+1] %>% gsub("^(\\s*).*","\\1", .) %>% nchar

  ## non-commented next node-name after type
  ncnn_id <- c()
  for (i in type_cid) {
    ncnn_id <- c(ncnn_id, nn_id[nn_id > i][1])
  }

  indent_next <- yl[ncnn_id] %>% gsub("^(\\s*).*","\\1", .) %>% nchar
  indent_issue <- indent_type == indent_next

  if (any(!indent_issue))
    err <- c(err, paste0("Indent issue in line(s): ", paste0(type_cid[!indent_issue] + 1, collapse = ", "), "\nUse the tab key to ensure a node name is indented the same amount\nas the node type on the preceding line."))

  ## check indent for node names
  indent_name <- yl[nn_id] %>% gsub("^(\\s*).*","\\1", .) %>% nchar

   ## check indent of next line for node names
  indent_next <- yl[nn_id+1] %>% gsub("^(\\s*).*","\\1", .) %>% nchar
  indent_issue <- indent_name >= indent_next
  if (any(indent_issue))
    err <- c(err, paste0("Indent issue in line(s): ", paste0(nn_id[indent_issue] + 1, collapse = ", "), "\nAlways use the tab key to indent the line(s) after specifying a node name."))

  ## determine return value
  if (length(err) > 0) {
    paste0("\n**\n", paste0(err, collapse = "\n"), "\n**\n") %>% set_class(c("dtree", class(.)))
  } else {
    paste0(yl, collapse = "\n")
  }
}

#' Create a decision tree
#'
#' @details See \url{http://vnijs.github.io/radiant/base/dtree.html} for an example in Radiant
#'
#' @param yl A yaml string or a list (e.g., from yaml::yaml.load_file())
#' @param opt Find the maximum ("max") or minimum ("min") value for each decision node
#'
#' @return A list with the initial tree and the calculated tree
#'
#' @importFrom yaml yaml.load
#' @importFrom stringr str_match
#' @importFrom data.tree as.Node Clone isLeaf isNotLeaf
#'
#' @seealso \code{\link{summary.dtree}} to summarize results
#' @seealso \code{\link{plot.dtree}} to plot results
#'
#' @export
dtree <- function(yl, opt = "max") {

  ## most of the code in this function is from
  ## https://github.com/gluc/useR15/blob/master/01_showcase/02_decision_tree.R

  ## load yaml from string if list not provide
  if (is_string(yl)) {

    ## get input file from r_data
    # if (!grepl("name\\s*:", yl)) yl <- getdata(yl)
    if (!grepl("\\n", yl)) yl <- getdata(yl)

    yl <- dtree_parser(yl)

    ####### test #######
    # return(paste0(paste0("\n**\n", yl, collapse = "\n"), "\n**\n") %>% set_class(c("dtree", class(.))))
    ####### test #######

    if (class(yl)[1] == "dtree") return(yl)

    # yl <- try(yaml.load(yl), silent = TRUE)

    ## if the name of input-list in r_data is provided
    # if (!is(yl, 'try-error') && is_string(yl)) yl <- try(yaml.load(getdata(yl)), silent = TRUE)
    yl <- try(yaml.load(yl), silent = TRUE)

    ## used when a string is provided
    if (is(yl, 'try-error')) {
      err_line <- stringr::str_match(attr(yl,"condition")$message, "^Scanner error:.*line\\s([0-9]*),")[2]
      if (is.na(err_line))
        err <- paste0("**\nError reading input:\n", attr(yl,"condition")$message, "\n\nPlease try again. Examples are shown in the help file\n**")
      else
        err <- paste0("**\nIndentation error in line ", err_line, ".\nUse tabs to separate the branches in the decision tree.\nFix the indentation error and try again. Examples are shown in the help file\n**")
      return(set_class(err, c("dtree",class(err))))
    }
  }

  if (length(yl) == 0) {
    err <- "**\nThe provided list is empty or not in the correct format.\nPlease check the input file.\n**"
    return(set_class(err, c("dtree",class(err))))
  }

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
  jl_init <- Clone(jl)

  ## Aggregate gives strange errors when used twice (see issue at gluc/data.tree)
  ## calculate payoff
  # calc_payoff <- function(x) {
  #   if (x$type == 'chance') x$payoff <- Aggregate(x, function(node) node$payoff * node$p, sum)
  #   else if (x$type == 'decision') x$payoff <- Aggregate(x, "payoff", max)

  #   ## subtract cost if specified
  #   if (!is.null(x$cost)) x$payoff <- x$payoff - x$cost
  # }

  chance_payoff <- function(node) {
    if (is.null(node$payoff) || is.null(node$p)) {
      0
    } else {
      # if (is_string(node$p)) node$p <- eval(parse(text = node$p))
      # node$payoff * node$p
      p <- if (is_string(node$p)) eval(parse(text = node$p)) else node$p
      node$payoff * p
    }
  }

  decision_payoff <- function(node)
    if(is.null(node$payoff)) 0 else node$payoff

  type_none <- ""
  calc_payoff <- function(x) {
    # if (x$type == 'chance') x$payoff <- sum(sapply(x$children, function(node) node$payoff * node$p))
    # if (is_empty(x$type)) x$payoff <- 0
    if (is_empty(x$type)) {
      x$payoff <- 0
      x$type <- "NONE"
      type_none <<- "One or more nodes do not have a 'type'. Search for 'NONE' in the output\nbelow and then update the input file"
    } else if (x$type == 'chance') x$payoff <- sum(sapply(x$children, chance_payoff))
    # else if (x$type == 'decision') x$payoff <- max(sapply(x$children, function(node) node$payoff))
    # else if (x$type == 'decision') x$payoff <- max(sapply(x$children, decision_payoff))
    else if (x$type == 'decision') x$payoff <- get(opt)(sapply(x$children, decision_payoff))

    ## subtract cost if specified
    if (!is.null(x$cost)) x$payoff <- x$payoff - x$cost
  }

  err <- try(jl$Do(calc_payoff, traversal = "post-order", filterFun = isNotLeaf), silent = TRUE)
  # jl$Do(calc_payoff, traversal = "post-order", filterFun = isNotLeaf)
  # err <- ""

  if (is(err, 'try-error')) {
    err <- paste0("**\nError calculating payoffs associated with a chance or decision node.\nPlease check that each terminal node has a payoff and that probabilities\nare correctly specificied\n**")
    return(err %>% set_class(c("dtree", class(.))))
  }

  decision <- function(x) {
    # po <- sapply(x$children, function(child) child$payoff)
    po <- sapply(x$children, decision_payoff)
    x$decision <- names(po[po == x$payoff])
  }

  # jl$Do(decision, filterFun = function(x) !is.null(x$type) && x$type == 'decision')
  # err <- ""
  err <- try(jl$Do(decision, filterFun = function(x) !is.null(x$type) && x$type == 'decision'), silent = TRUE)

  if (is(err, 'try-error')) {
    err <- paste0("**\nError calculating payoffs associated with a decision node. Please check\nthat each terminal node has a payoff\n**")
    return(err %>% set_class(c("dtree", class(.))))
  }

  list(jl_init = jl_init, jl = jl, type_none = type_none) %>% set_class(c("dtree",class(.)))
}

#' Summary method for the dree function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/dtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{simulater}}
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom data.tree Traverse Get FormatPercent
#'
#' @seealso \code{\link{dtree}} to generate the results
#' @seealso \code{\link{plot.dtree}} to plot results
#'
#' @export
summary.dtree <- function(object, ...) {

  if (is.character(object)) return(cat(object))

  print_money <- function(x) {
    x %>% {if (is.na(.)) "" else .} %>%
      format(digits = 10, nsmall = 2, decimal.mark = ".", big.mark = ",", scientific = FALSE)
  }

  print_percent <- function(x) {
    if (is_string(x)) x <- eval(parse(text = x))
    # print(x)
    # if (!is.numeric(x)) x <- 0
    FormatPercent(x)
  }

  rm_terminal <- function(x)
    x %>% {if (is.na(.)) "" else .} %>% {if (. == "terminal") "" else .}

  # format_percent <- . %>% as.character %>% sprintf("%.2f%%", . * 100)

  format_dtree <- function(jl) {
    ## set parent type
    nt <- jl$Get(function(x) x$parent$type)
    jl$Set(ptype = nt)

    Traverse(jl) %>%
      {data.frame(
        ` ` = Get(.,"levelName"),
        # Probability = Get(., "p", format = FormatPercent),
        Probability = Get(., "p", format = print_percent),
        Payoff = Get(., "payoff", format = print_money),
        Cost = Get(., "cost", format = print_money),
        Type = Get(., "ptype", format = rm_terminal),
        check.names = FALSE
      )
    } %>% { .[[" "]] <- format(.[[" "]], justify = "left"); .}
  }

  ## initial setup
  if (object$type_none != "") {
    cat(paste0("\n\n**\n",object$type_none,"\n**\n\n"))
  } else {
    cat("Initial decision tree:\n")
    format_dtree(object$jl_init) %>% print(row.names = FALSE)
  }

  cat("\n\nFinal decision tree:\n")
  format_dtree(object$jl) %>% print(row.names = FALSE)


  # cat("\n\nDecision:\n")
  # object$jl$Get("decision") %>% .[!is.na(.)] %>% paste0(collapse = " & ") %>% cat
}

#' Plot method for the dtree function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/dtree.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{dtree}}
#' @param symbol Monetary symbol to use ($ is the default)
#' @param dec Decimal places to round results to
#' @param final If TRUE plot the decision tree solution, else the initial decision tree
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom data.tree Traverse Get isNotRoot
#'
#' @seealso \code{\link{dtree}} to generate the result
#' @seealso \code{\link{summary.dtree}} to summarize results
#'
#' @export
plot.dtree <- function(x, symbol = "$", dec = 3, final = FALSE, shiny = FALSE, ...) {

  # if (is.character(x)) return(cat(x))
  if (is.character(x)) return(paste0("graph LR\n A[Errors in the input file]\n"))
  if (x$type_none != "") return(paste0("graph LR\n A[Node does not have a type. Fix the input file]\n"))

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

    # print("decision")
    # print(node$parent$decision)
    # print("name")
    # print(node$name)

    # if (!is.null(node$parent$decision) && !is.null(node$name) && node$name == node$parent$decision)
    if (length(node$parent$decision) > 0 && length(node$name) > 0 && node$name == node$parent$decision)
      paste0(" === |", lbl, "|")
    else
      paste0(" --- |", lbl, "|")
  }

  FormatPayoff <- function(payoff) {
    if (is.null(payoff)) payoff <- 0
    paste0(symbol, format(round(payoff, dec), scientific = FALSE, big.mark = ","))
  }

  ToLabel <- function(node) {
    po <- if (final) FormatPayoff(node$payoff) else " "
    if (node$type == "decision") {
      lbl <- paste0("[", po, "]")
    } else if (node$type == "chance") {
      lbl <- paste0("((", po, "))")
    } else if (node$type == "terminal") {
      lbl <- paste0("[", FormatPayoff(node$payoff), "]")
      # lbl <- paste0("[<span title='test'>", FormatPayoff(node$payoff), "</span>]")
    }
    paste0(" ", node$id, lbl)
  }

  style_decision <- jl$Get("id", filterFun = function(x) x$type == "decision")
  if (is.null(style_decision)) style_decision <- "id_null"
  style_chance <- jl$Get("id", filterFun = function(x) x$type == "chance")
  if (is.null(style_chance)) style_chance <- "id_null"

  style <- paste0(
    "classDef default fill:none, bg:none, stroke-width:0px;
    classDef chance fill:#FF8C00,stroke:#333,stroke-width:1px;
    classDef decision fill:#9ACD32,stroke:#333,stroke-width:1px;
    class ", paste(style_decision, collapse = ","), " decision;
    class ", paste(style_chance, collapse = ","), " chance;")

  trv <- Traverse(jl, traversal = "level", filterFun = isNotRoot)
  df <- data.frame(from = Get(trv, FromLabel), edge = Get(trv, EdgeLabel), to = Get(trv, ToLabel))

  paste("graph LR", paste( paste0(df$from,df$edge, df$to), collapse = "\n"),
    style, sep = "\n") %>%
    {if (shiny) . else DiagrammeR::DiagrammeR(.)}
}

    # "click id1 callback 'Tooltip';\n"

## some initial ideas for sensitivity analysis
# library(yaml); library(radiant)
# library(radiant); library(data.tree)
# yl <- yaml::yaml.load_file("~/Dropbox/teaching/MGT403-2015/data.tree/jennylind-variables.yaml")
# eval(parse(text = yl$variables))
# p_medium
# p_large

# names(yl)

# yl$test <- function(x) x + 1
# yl$test(2)
# yl <- yaml::yaml.load_file("~/Dropbox/teaching/MGT403-2015/data.tree/quant_job.yaml")
# dtree(yl)
# dtree(yl)
# dtree(yl) %>% summary
# dtree(yl) %>% plot


# dtree(yl) %>% plot(final = TRUE)
# dtree(yl) %>% plot(final = FALSE)
# yl <- yaml::yaml.load_file("~/Dropbox/teaching/MGT403-2015/data.tree/quant_job.yaml")
# dtree(yl) %>% summary
# dtree(yl) %>% plot
# dtree(yl) %>% plot(final = TRUE)

# shiny::runApp("~/gh/radiant/inst/quant")

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


