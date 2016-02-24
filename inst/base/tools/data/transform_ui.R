## UI-elements for transform
output$ui_tr_vars <- renderUI({
  vars <- varnames()
  if (not_available(vars)) return()
  selectInput("tr_vars", "Select variable(s):", choices  = vars,
    multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
})

output$ui_tr_replace <- renderUI({
  vars <- varnames()
  selectInput("tr_replace", "Select replacement variables:", choices  = vars,
    multiple = TRUE, size = min(2, length(vars)), selectize = FALSE)
})

output$ui_tr_normalizer <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- varnames()[isNum]
  if (length(vars) == 0) return(NULL)
  selectInput("tr_normalizer", "Normalizing variable:", c("None" = "none", vars),
              selected = "none")
})

output$ui_tr_tab2dat <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- varnames()[isNum]
  # if (length(vars) == 0) return(NULL)
  selectInput("tr_tab2dat", "Frequency variable:", c("None" = "none", vars),
              selected = "none")
})

output$ui_tr_reorg_vars <- renderUI({
  ## need a dependency to reset list of variables
  if (is_empty(input$tr_change_type)) return()
  vars <- varnames()
  selectizeInput("tr_reorg_vars", "Reorder/remove variables:", choices  = vars,
    selected = vars, multiple = TRUE,
    options = list(placeholder = 'Select variable(s)',
                   plugins = list('remove_button', 'drag_drop')))
})

output$ui_tr_reorg_levs <- renderUI({
  ## need a dependency to reset levels
  if (is_empty(input$tr_change_type)) return()
	if (not_available(input$tr_vars)) return()
  fctCol <- input$tr_vars[1]
	isFct <- "factor" == .getclass()[fctCol]
  if (!isFct) return()
  levs <- .getdata_transform()[[fctCol]] %>% levels

  selectizeInput("tr_reorg_levs", "Reorder/remove levels:", choices  = levs,
    selected = levs, multiple = TRUE,
    options = list(placeholder = 'Select level(s)',
                   plugins = list('remove_button', 'drag_drop')))
})

output$ui_tr_log <- renderUI({
  tagList(
    "<label>Transform command log:</label><br>" %>% HTML,
    tags$textarea(state_init("tr_log",""), id = "tr_log", type = "text", rows = 5, class = "form-control")
  )
})

ext_options <- list("none" = "", "log" = "_ln", "exp" = "_exp",
                    "square" = "_sq", "sqrt" = "_sqrt", "center" = "_ct",
                    "standardize" = "_st", "inverse" = "_inv")

output$ui_tr_ext <- renderUI({
  trfun <- input$tr_transfunction
  if (is_empty(trfun)) trfun <- "none"
  ext <- ext_options[[trfun]]
  returnTextInput("tr_ext", "Variable name extension:", ext)
})

output$ui_tr_ext_nz <- renderUI({
  if (is_empty(input$tr_normalizer, "none")) return()
  ext <- paste0("_", input$tr_normalizer)
  returnTextInput("tr_ext_nz", "Variable name extension:", ext)
})

output$ui_tr_rcname <- renderUI({
  if (is_empty(input$tr_vars)) return()
  rcname <- paste0(input$tr_vars, "_rc")
  returnTextInput("tr_rcname", "Recoded variable name:", rcname)
})

output$ui_tr_ext_bin <- renderUI({
  if (is_empty(input$tr_vars)) return()
  returnTextInput("tr_ext_bin", "Variable name extension:", "_dec")
})

output$ui_tr_roname <- renderUI({
  if (is_empty(input$tr_vars)) return()
  returnTextInput("tr_roname", "Variable name:", input$tr_vars[1])
})

output$ui_tr_typename <- renderUI({
  if (is_empty(input$tr_vars)) return()
  returnTextInput("tr_typename", "Variable name extension:", "")
})

output$ui_tr_dataset <- renderUI({
  tr_dataset <- input$dataset
  if (input$tr_change_type == "show_dup") {
    tr_dataset <- paste0(tr_dataset, "_dup")
  } else if (input$tr_change_type == "holdout") {
    tr_dataset <- paste0(tr_dataset, "_holdout")
  } else if (input$tr_change_type == "tab2dat") {
    tr_dataset <- paste0(tr_dataset, "_dat")
  }
  tags$table(
    tags$td(textInput("tr_dataset", "Store changes in:", tr_dataset)),
    tags$td(actionButton("tr_store", "Store"), style="padding-top:30px;")
  )
})

trans_options <- list("None" = "none", "Ln (natural log)" = "log", "Exp" = "exp",
                      "Square" = "square", "Square-root" = "sqrt",
                      "Center" = "center", "Standardize" = "standardize", "Inverse" = "inverse")

type_options <- list("None" = "none", "As factor" = "as_factor",
                     "As numeric" = "as_numeric", "As integer" = "as_integer",
                     "As character" = "as_character", "As date (mdy)" = "as_mdy",
                     "As date (dmy)" = "as_dmy", "As date (ymd)" = "as_ymd",
                     "As date/time (mdy_hms)" = "as_mdy_hms",
                     "As date/time (mdy_hm)" = "as_mdy_hm",
                     "As date/time (dmy_hms)" = "as_dmy_hms",
                     "As date/time (dmy_hm)" = "as_dmy_hm",
                     "As date/time (ymd_hms)" = "as_ymd_hms",
                     "As date/time (ymd_hm)" = "as_ymd_hm")

trans_types <- list("None" = "none", "Type" = "type", "Transform" = "transform",
                    "Create" = "create", "Bin" = "bin", "Clipboard" = "clip",
                    "Normalize" = "normalize",
                    "Recode" = "recode",
                    "Rename" = "rename", "Replace" = "replace",
                    "Reorder/remove levels" = "reorg_levs",
                    "Reorder/remove variables" = "reorg_vars",
                    "Remove missing values" = "remove_na",
                    "Remove duplicates" = "remove_dup",
                    "Show duplicates" = "show_dup",
                    "Training variable" = "training",
                    "Holdout sample" = "holdout",
                    "Table-to-data" = "tab2dat")

output$ui_Transform <- renderUI({
	## Inspired by Ian Fellow's transform ui in JGR/Deducer
  tagList(wellPanel(
    checkboxInput("tr_hide", "Hide summaries", state_init("tr_hide", FALSE)),
    uiOutput("ui_tr_vars"),
    selectInput("tr_change_type", "Transformation type:", trans_types, selected = "none"),
    conditionalPanel(condition = "input.tr_change_type == 'type'",
	    selectInput("tr_typefunction", "Change variable type:", type_options, selected = "none")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'transform'",
	    selectInput("tr_transfunction", "Apply function:", trans_options)
    ),
    conditionalPanel(condition = "input.tr_change_type == 'normalize'",
      uiOutput("ui_tr_normalizer")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'tab2dat'",
      uiOutput("ui_tr_tab2dat")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'create'",
	    returnTextAreaInput("tr_create", "Create (e.g., x = y - z):", "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'bin'",
      numericInput("tr_bin_n", label = "Nr bins:", min = 2, value = 10),
      checkboxInput("tr_bin_rev", "Reverse order", value = FALSE),
      uiOutput("ui_tr_ext_bin")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'training'",
      tags$table(
        tags$td(numericInput("tr_training_n", label = "Size:", min = 0, value = .7)),
        tags$td(textInput("tr_training", "Variable name:", "training"))
      )
    ),
    conditionalPanel(condition = "input.tr_change_type == 'holdout'",
      checkboxInput("tr_holdout_rev", "Reverse filter", value = TRUE)
    ),
    conditionalPanel(condition = "input.tr_change_type == 'clip'",
    	HTML("<label>Paste from spreadsheet:</label>"),
    	tags$textarea(class="form-control", id="tr_paste", rows=3, "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'recode'",
	    returnTextAreaInput("tr_recode", "Recode (e.g., lo:20 = 1):", "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'rename'",
      returnTextAreaInput("tr_rename", "Rename (separate by , ):", "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'replace'",
      uiOutput("ui_tr_replace")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'reorg_vars'",
      uiOutput("ui_tr_reorg_vars")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'reorg_levs'",
      uiOutput("ui_tr_reorg_levs")
    ),
    conditionalPanel("input.tr_change_type == 'transform'",
      uiOutput("ui_tr_ext")
    ),
    conditionalPanel("input.tr_change_type == 'recode'",
      uiOutput("ui_tr_rcname")
    ),
    conditionalPanel("input.tr_change_type == 'normalize'",
      uiOutput("ui_tr_ext_nz")
    ),
    conditionalPanel("input.tr_change_type == 'reorg_levs'",
      uiOutput("ui_tr_roname")
    ),
    conditionalPanel("input.tr_change_type == 'type'",
      uiOutput("ui_tr_typename")
    ),
    conditionalPanel(condition = "input.tr_change_type != 'none'",
      uiOutput("ui_tr_dataset")
	  )),
    help_and_report(modal_title = "Transform",
                    fun_name = "transform",
                    help_file = inclMD(file.path(r_path, "base/tools/help/transform.md")))
	)
})

.change_type <- function(dataset, fun,
                         vars = "",
                         ext = "",
                         store_dat = "",
                         store = TRUE) {

  if (!store || !is.character(dataset)) {
    fun <- get(fun)
    if (is_empty(ext)) {
      mutate_each_(dataset, funs(fun), vars)
    } else {
      mutate_each_(dataset, funs(fun), vars) %>% set_colnames(paste0(vars, ext))
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(ext)) {
      paste0("## change variable type\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(", fun, "), ", paste0(vars, collapse = ", "),")\n")
    } else {
      paste0("## change variable type\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(", fun, "), ext = \"", ext, "\", ", paste0(vars, collapse = ", "), ")\n")
    }
  }
}

.transform <- function(dataset, fun,
                       vars = "",
                       ext = "",
                       store_dat = "",
                       store = TRUE) {

  if (!store && !is.character(dataset)) {
    fun <- get(fun)
    if (is_empty(ext)) {
      mutate_each_(dataset, funs(fun), vars)
    } else {
      mutate_each_(dataset, funs(fun), vars) %>% set_colnames(paste0(vars, ext))
    }
  } else {

    if (store_dat == "") store_dat <- dataset
    if (is_empty(ext)) {
      paste0("## transform variable\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(", fun, "), ", paste0(vars, collapse = ", "), ")\n")
    } else {
      paste0("## transform variable\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(", fun, "), ext = \"", ext, "\", ", paste0(vars, collapse = ", "), ")\n")
    }
  }
}

## ensure no variables are selected 'by accident' when creating a new variable
observeEvent(input$tr_change_type, {
  if (input$tr_change_type == "create")
    updateSelectInput(session = session, inputId = "tr_vars", selected = character(0))
})

.create <- function(dataset, cmd,
                    byvar = "",
                    store_dat = "",
                    store = TRUE) {

  if (!store || !is.character(dataset)) {
    if (gsub("\\s","",cmd) == "") return(dataset)

    cmd <- cmd %>% gsub("\"","\'",.) %>% gsub(" ","",.)
    vars <-
      strsplit(cmd, ";")[[1]] %>% strsplit(., "=") %>%
      sapply("[", 1) %>% gsub(" ","",.)
    dots <- strsplit(cmd, ";")[[1]] %>% gsub(" ","",.)
    for (i in seq_along(dots))
      dots[[i]] <- sub(paste0(vars[[i]],"="),"",dots[[i]])

    if (is_empty(byvar)) {
      ## using within and do.call because it provides better err messages
      nvar <- try(do.call(within, list(dataset, parse(text = cmd))), silent = TRUE)
      # nvar <- try(mutate_(dataset, .dots = setNames(dots, vars)), silent = TRUE)
    } else {
      dots <- sub("^","~",dots) %>% lapply(as.formula, env = r_env)
      nvar <- try(group_by_(dataset, .dots = byvar) %>% mutate_(.dots = setNames(dots, vars)), silent = TRUE)
    }
    if (is(nvar, 'try-error')) {
      paste0(" **\nThe create command was not valid. The command entered was:\n\n", cmd, "\n\nThe error message was:\n\n", attr(nvar,"condition")$message, "\n\nPlease try again. Examples are shown in the help file\n**")
    } else {
      select_(nvar, .dots = vars) %>% ungroup
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(byvar))
      paste0("## create new variable(s)\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", gsub(";",",",cmd), ")\n")
    else
      paste0("## create new variable(s)\nr_data[[\"",store_dat,"\"]] <- group_by(r_data[[\"",dataset,"\"]], ", paste0(byvar, collapse = ", "), ") %>% mutate(", gsub(";",",",cmd), ") %>% ungroup\n")
  }
}

.recode <- function(dataset, var, cmd,
                    rcname = "",
                    store_dat = "",
                    store = TRUE) {

  cmd <- cmd %>% gsub("\\n","", .) %>% gsub("\"","\'",.)
  if (is_empty(rcname)) rcname <- paste0(var, "_rc")

  if (!store || !is.character(dataset)) {
    if (cmd == "") return(dataset)
    nvar <- try(car::recode(dataset[[var]], cmd), silent = TRUE)
    if (is(nvar, 'try-error')) {
      paste0("The recode command was not valid. The error message was:\n", attr(nvar,"condition")$message, "\nPlease try again. Examples are shown in the help file (click the ? icon).")
    } else {
      data.frame(nvar) %>% setNames(rcname)
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## recode variable\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", rcname, " = recode(", var, ", \"", cmd, "\"))\n")
  }
}

.rename <- function(dataset, var, rnm,
                    store_dat = "",
                    store = TRUE) {

  rnm <- rnm %>% gsub("\\s","", .) %>% gsub(";",",", .)
  if (gsub("\\s","",rnm) != "") rnm <- unlist(strsplit(rnm, ",")) %>% .[1:min(length(.),length(var))]

  if (!store || !is.character(dataset)) {
    if (rnm[1] == "") return(dataset)
    names(dataset)[1:length(rnm)] <- rnm %>% gsub("^\\s+|\\s+$", "", .)
    ## dplyr alternative has the same dplyr::changes result
    # rename_(dat, .dots = setNames(l2,l1))
    dataset
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## rename variable(s)\nr_data[[\"",store_dat,"\"]] <- rename(r_data[[\"",dataset,"\"]], ", paste(rnm, var, sep = " = ", collapse = ", "), ")\n")
  }
}

.replace <- function(dataset, var, rpl,
                     store_dat = "",
                     store = TRUE) {

  if (!store || !is.character(dataset)) {
    select_(dataset, .dots = rpl) %>% set_colnames(var)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## replace variable(s)\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", paste(var, rpl, sep = " = ", collapse = ", "), ") %>% select(", paste0("-",rpl, collapse = ", "),")\n")
  }
}

.normalize <- function(dataset, vars, nzvar,
                       ext = paste0("_",nzvar),
                       store_dat = "",
                       store = TRUE) {

  if (!store && !is.character(dataset)) {
    nz <- select_(dataset, .dots = nzvar)
    dat <- select_(dataset, .dots = vars)
    dc <- getclass(dat)

    isnum <- "numeric" == dc | "integer" == dc
    if (sum(isnum) == 0) return("Please select only integer or numeric variables to normalize")
    vars <- vars[isnum]
    select_(dat, .dots = vars) %>% {. / nz[[1]]} %>%
      set_colnames(paste0(vars, ext))
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## normalize variables\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(normalize(.,",nzvar,")), ext = \"", ext, "\", ", paste0(vars, collapse = ", "), ")\n")
  }
}

.tab2dat <- function(dataset, freq,
                     vars = "",
                     store_dat = "",
                     store = TRUE) {

  if (!store && !is.character(dataset)) {
    if (is_empty(vars)) vars <- setdiff(colnames(dataset),freq)
    select_(dataset, .dots = unique(c(vars, freq))) %>%
    table2data(freq)
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(vars)) vars <- setdiff(colnames(dataset),freq)
    paste0("## Create data from a table\nr_data[[\"",store_dat,"\"]] <- select(r_data[[\"",dataset,"\"]], ", paste0(vars, collapse = ", "), ") %>% table2data('",freq,"')\n")
  }
}

.bin <- function(dataset,
                 vars = "",
                 bins = 10,
                 rev = FALSE,
                 ext = "_dec",
                 store_dat = "",
                 store = TRUE) {

  if (!store && !is.character(dataset)) {
    if (is.na(bins) || !is.integer(bins)) return("Please specify the (integer) number of bins to use")
    xt <- function(x, bins, rev) radiant::xtile(x, bins, rev = rev)
    select_(dataset, .dots = vars) %>%
    # mutate_each(funs(radiant::xtile(.,bins, rev = rev))) %>%
    mutate_each(funs(xt(., bins, rev = rev))) %>%
    set_colnames(paste0(vars, ext))
  } else {
    if (store_dat == "") store_dat <- dataset
    if (rev)
      paste0("## bin variables\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(xtile(.,",bins,", rev = TRUE)), ext = \"", ext, "\", ", paste0(vars, collapse = ", "), ")\n")
    else
      paste0("## bin variables\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(xtile(.,",bins,")), ext = \"", ext, "\", ", paste0(vars, collapse = ", "), ")\n")
  }
}

.training <- function(dataset,
                      vars = "",
                      n = .7,
                      nr = 100,
                      name = "training",
                      store_dat = "",
                      store = TRUE) {

  if (is_empty(name)) name <- "training"
  if (!store && !is.character(dataset)) {
    n <- n %>% {ifelse (. < 0 || is.na(.) || . > nr, .7, .)}
    if (is_empty(vars)) {
      data.frame(make_train(n, nr)) %>% setNames(name)
    } else {
      dat <- dataset %>% group_by_(.dots = vars)
      nr <- length(attr(dat, "indices"))
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## created variable to select training sample\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", name, " = make_train(", n, ", n()))\n")
  }
}

## Make a training variable that selects randomly by ID
# http://rpackages.ianhowson.com/cran/dplyr/man/group_indices.html
## Make a training variable that selects randomly within ID
# http://rpackages.ianhowson.com/cran/dplyr/man/sample.html
## search for stratified sampling and cluster sampling
## link to sampling menu?

.reorg_levs <- function(dataset, fct, levs,
                        name = fct,
                        store_dat = "",
                        store = TRUE) {

  if (is_empty(name)) name <- fct
  if (!store || !is.character(dataset)) {
    data.frame(factor(dataset[[fct]], levels = levs)) %>% setNames(name)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## change factor levels\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", name, " = factor(", fct, ", levels = c(\"", paste0(levs, collapse = "\",\""), "\")))\n")
  }
}

.reorg_vars <- function(dataset,
                        vars = "",
                        store_dat = "",
                        store = TRUE) {

  if (!store || !is.character(dataset)) {
    getdata(dataset, vars, filt = "", na.rm = FALSE)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## reorder/remove variables\nr_data[[\"",store_dat,"\"]] <- select(r_data[[\"",dataset,"\"]], ", paste0(vars, collapse = ", "),")\n")
  }
}

.remove_na <- function(dataset,
                       vars = "",
                       store_dat = "",
                       nr_col = 0,
                       store = TRUE) {

  if (!store || !is.character(dataset)) {
    if (all(vars == "") || length(unique(vars)) == ncol(dataset)) {
      dataset %>% filter(complete.cases(.))
    } else {
      ind <- select_(dataset, .dots = vars) %>% complete.cases
      filter(dataset, ind)
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    if (all(vars == "") || length(unique(vars)) == nr_col) vars <- "."
    paste0("## remove missing values\nr_data[[\"",store_dat,"\"]] <- r_data[[\"",dataset,"\"]] %>% filter(complete.cases(", vars, "))\n")
  }
}

.remove_dup <- function(dataset,
                        vars = "",
                        store_dat = "",
                        nr_col = 0,
                        store = TRUE) {

  if (!store || !is.character(dataset)) {
    if (all(vars == "") || length(unique(vars)) == ncol(dataset))
      dat <- distinct(dataset)
    else
      dat <- distinct_(dataset, .dots = vars)

    if (nrow(dat) == nrow(dataset))
      paste0("No duplicates found (n_distinct = ", nrow(dat),")")
    else
      dat
  } else {
    if (all(vars == "") || length(unique(vars)) == nr_col)
      paste0("## remove duplicate rows\nr_data[[\"",store_dat,"\"]] <- distinct(r_data[[\"",dataset,"\"]])\n")
    else
      paste0("## remove rows with duplicate values\nr_data[[\"",store_dat,"\"]] <- distinct(r_data[[\"",dataset,"\"]], ", paste0(vars, collapse = ", "),")\n")
  }
}

.show_dup <- function(dataset,
                      vars = "",
                      store_dat = "",
                      nr_col = 0,
                      store = TRUE) {

  if (!store || !is.character(dataset)) {
    if (all(vars == "") || length(unique(vars)) == ncol(dataset)) {
      dat <- filter(dataset, duplicated(dataset))
    } else {
      dat <- dataset %>% group_by_(.dots = vars) %>%
        filter(n() > 1)

      if (nrow(dat) > 0) {
        dat <- mutate(dat, nr_dup = 1:n()) %>%
        arrange_(.dots = vars) %>%
        ungroup
      }
    }

    if (nrow(dat) == 0)
      ## "No duplicates found"
      paste0("No duplicates found (n_distinct = ", nrow(dataset),")")
    else
      dat
  } else {
    if (all(vars == "") || length(unique(vars)) == nr_col)
      paste0("## show duplicate rows\nr_data[[\"",store_dat,"\"]] <- r_data[[\"",dataset,"\"]] %>% filter(duplicated(.))\n")
    else
      paste0("## show rows with duplicate values\nr_data[[\"",store_dat,"\"]] <- show_duplicated(r_data[[\"",dataset,"\"]], ", paste0(vars, collapse = ", "),")\n")
  }
}

.holdout <- function(dataset,
                     vars = "",
                     filt = "",
                     rev = "",
                     store_dat = "",
                     store = TRUE) {

  if (rev) filt <- paste0("!(",filt,")")

  if (!store || !is.character(dataset)) {
    if (is_empty(filt))
      return(paste0("No filter found (n = ", nrow(dataset),")"))

    getdata(dataset, vars = vars, filt = filt, na.rm = FALSE)

  } else {
    filt <- gsub("\"","'",filt)
    if (all(vars == ""))
      paste0("## create holdout sample\nr_data[[\"",store_dat,"\"]] <- getdata(\"",dataset,"\", filt = \"", filt, "\", na.rm = FALSE)\n")
    else
      paste0("## create holdout sample\nr_data[[\"",store_dat,"\"]] <- getdata(\"",dataset,"\", filt = \"", filt, "\", na.rm = FALSE) %>% select(", paste0(vars, collapse = ", "),")\n")
  }
}

# .sort ......
# arrange_(mtcars, .dots = c("desc(cyl)","mpg"))

inp_vars <- function(inp, rval = "")
	if (is_empty(input[[inp]])) rval else input[[inp]]

transform_main <- reactive({

  req(input$tr_change_type)
	# if (is.null(input$tr_change_type)) return()

  if (not_available(input$tr_vars)) {
    if (input$tr_change_type == "none") {
      return("Select a transformation type and select one or more variables to transform")
    } else if (input$tr_change_type == "type") {
      return("Select one or more variables to change their variable type")
    } else if (input$tr_change_type == "transform") {
      return("Select one or more variables to apply a transformation")
    } else if (input$tr_change_type == "rename") {
      return("Select one or more variables to rename")
    } else if (input$tr_change_type == "replace") {
      return("Select one or more variables to replace")
    } else if (input$tr_change_type == 'recode') {
      return("Select a variable to recode")
    } else if (input$tr_change_type == "bin") {
      return("Select one or more variables to bin")
    } else if (input$tr_change_type == 'reorg_levs') {
      return("Select a variable of type factor to change the ordering and/or number of levels")
    } else if (input$tr_change_type == 'normalize') {
      return("Select one or more variables to normalize")
    } else if (input$tr_change_type == "remove_na") {
      return("Select a one or more variables to see the effects of removing missing values")
    } else if (input$tr_change_type %in% c("remove_dup","show_dup")) {
      return("Select a one or more variables to see the effects of removing duplicates")
    }
  }

  ## get the active dataset, filter not applied when called from transform tab
  dat <- .getdata_transform()

  ## what data to pass on ...
	if (input$tr_change_type == "none")
 		return(select_(dat, .dots = input$tr_vars))

  ## reorganize variables
	if (input$tr_change_type == "reorg_vars")
 	  return(.reorg_vars(dat, inp_vars("tr_reorg_vars"), store = FALSE))

  ## create training variable
  if (input$tr_change_type == 'training')
    return(.training(dat, n = input$tr_training_n, nr = nrow(dat), name = input$tr_training, store = FALSE))

  if (input$tr_change_type == 'create') {
    if (input$tr_create == "") {
      return("Specify an equation to create a new variable and press 'return'. **\n** See the help file for examples")
    } else {
      return(.create(dat, input$tr_create, byvar = inp_vars("tr_vars"), store = FALSE))
    }
  }

  if (input$tr_change_type == 'tab2dat') {
    if (is.null(input$tr_tab2dat) || input$tr_tab2dat == "none") {
      return("Select a frequency variable")
    } else if (!is_empty(input$tr_vars) && all(input$tr_vars == input$tr_tab2dat)) {
      return("Select at least one variable that is not the frequency variable")
    } else {
      req(available(input$tr_tab2dat))
      return(.tab2dat(dat, input$tr_tab2dat, vars = inp_vars("tr_vars"), store = FALSE))
    }
  }

  if (input$tr_change_type == 'clip') {
    if (input$tr_paste == "") {
      return("Copy-and-paste data with a header row from a spreadsheet")
    } else {
      cpdat <- try(read.table(header = TRUE, comment.char = "", fill = TRUE, sep = "\t", as.is = TRUE, text = input$tr_paste), silent = TRUE)
      if (is(cpdat, 'try-error')) {
        return("The pasted data was not well formated. Please make sure the number of rows **\n** in the data in Radiant and in the spreadsheet are the same and try again.")
      } else if (nrow(cpdat) != nrow(dat)) {
        return("The pasted data does not have the correct number of rows. Please make sure **\n** the number of rows in the data in Radiant and in the spreadsheet are the **\n** same and try again.")
      } else {
        return(cpdat)
      }
    }
  }

  ## filter data for holdout
  if (input$tr_change_type == "holdout") {
    if (!input$show_filter) return("No filter active. Click the 'Filter' checkbox and enter a filter")
    return(.holdout(dat, inp_vars("tr_vars"), filt = input$data_filter, rev = input$tr_holdout_rev, store = FALSE))
  }

  ## only use the functions below if variables have been selected
  if (!is_empty(input$tr_vars)) {
    if (not_available(input$tr_vars)) return()

    ## remove missing values
    if (input$tr_change_type == "remove_na")
      return(.remove_na(dat, inp_vars("tr_vars"), store = FALSE))

    ## bin variables
    if (input$tr_change_type == "bin")
      return(.bin(dat, inp_vars("tr_vars"), bins = input$tr_bin_n, rev = input$tr_bin_rev, ext = input$tr_ext_bin, store = FALSE))
             # bin_order = input$tr_bin_order, store = FALSE))

    ## remove duplicates
    if (input$tr_change_type == "remove_dup")
      return(.remove_dup(dat, inp_vars("tr_vars"), store = FALSE))

    ## show duplicates
    if (input$tr_change_type == "show_dup")
      return(.show_dup(dat, inp_vars("tr_vars"), store = FALSE))


    if (input$tr_change_type == 'normalize') {
      if (is_empty(input$tr_normalizer, "none")) {
        return("Select a normalizing variable")
      } else {
        return(.normalize(dat, inp_vars("tr_vars"), input$tr_normalizer, ext = input$tr_ext_nz, store = FALSE))
      }
    }

    if (input$tr_change_type == 'replace') {
      vars <- input$tr_vars
      rpl  <- input$tr_replace
      if (available(rpl)) {
        if (length(vars) != length(rpl))
          return(paste0("The number of replacement variables (", length(rpl), ") is not equal to the number of variables to replace (", length(vars),")"))
        return(.replace(dat, vars, rpl, store = FALSE))
      } else {
        return("Select one or more variable replacements")
      }
    }

    ## selecting the columns to show
		dat  <- select_(dat, .dots = input$tr_vars)
    vars <- colnames(dat)

    ## change in type is always done in-place
    if (input$tr_change_type ==  "type") {
      if (input$tr_typefunction == "none")
        return("Select a transformation type for the selected variables")
      else
        return(.change_type(dat, input$tr_typefunction, inp_vars("tr_vars"), input$tr_typename, store = FALSE))
    }

    ## change in type is always done in-place
    if (input$tr_change_type ==  "transform") {
      if (input$tr_transfunction == "none")
        return("Select a function to apply to the selected variable(s)")
      else
        return(.transform(dat, input$tr_transfunction, inp_vars("tr_vars"), input$tr_ext, store = FALSE))
    }

    if (input$tr_change_type == 'reorg_levs') {
        fct <- input$tr_vars[1]
        if (is.factor(dat[[fct]])) {
          return(.reorg_levs(dat, fct, input$tr_reorg_levs, input$tr_roname, store = FALSE))
        } else {
          return("Select a variable of type factor to change the ordering and/or number of levels")
        }
    }

    if (input$tr_change_type ==  'recode') {
      if (input$tr_recode == "") {
        return("Specify a recode statement, assign a name to the recoded variable, and press 'return'. **\n** See the help file for examples")
      } else {
        return(.recode(dat, inp_vars("tr_vars")[1], input$tr_recode, input$tr_rcname, store = FALSE))
      }
    }

    if (input$tr_change_type == 'rename') {
      if (input$tr_rename == "") {
        return("Specify new names for the selected variables (separated by a ',') and press 'return'")
      } else {
        if (any(input$tr_rename %in% varnames())) {
          return("One or more of the new variables names already exists in the data. **\n** Change the specified names or use the Replace function")
        } else {
          return(.rename(dat, inp_vars("tr_vars"), input$tr_rename, store = FALSE))
        }
      }
    }
	}

  return(invisible())
})

output$transform_data <- reactive({
  dat <- transform_main()
  if (is.null(dat) || is.character(dat) || nrow(dat) == 0 || ncol(dat) == 0) {
    tr_snippet()
  } else {
    show_data_snippet(dat)
  }
})

tr_snippet <- reactive({
  show_data_snippet(.getdata_transform())
})

output$transform_summary <- renderPrint({
  req(input$tr_hide == FALSE)

 	dat <- transform_main()
  ## with isolate on the summary wouldn't update when the dataset was changed
  if (is.null(dat)) return(invisible())
  if (is.character(dat)) {
    cat("**", dat,"**\n\n")
  } else {
    if (min(dim(dat)) == 0) {
      cat("** The selected operation resulted in an empty data frame and cannot be executed **\n\n")
    } else {
      if (input$tr_change_type == "none") {
        cat("** Select a transformation type **\n\n")
      } else {
        cat("** Press the 'Store' button to add your changes to the data **\n\n")
        if (!is_empty(input$tr_vars) && input$tr_change_type == "create")
          cat("** Results are grouped by", paste(input$tr_vars, collapse = ", "), "**\n\n")
      }

      cat(paste0(capture.output(getsummary(dat)), collapse = "\n"))
    }
  }
})

observeEvent(input$tr_store, {
	dat <- transform_main()
	if (is.null(dat)) return()
	if (is.character(dat)) return()
  if (min(dim(dat)) == 0) return()

	## saving to a new dataset if specified
	dataset <- input$tr_dataset
  ncmd <- ""
	if (is.null(r_data[[dataset]])) {
    r_data[[dataset]] <- .getdata_transform()
		r_data[[paste0(dataset,"_descr")]] <- r_data[[paste0(input$dataset,"_descr")]]
		r_data[['datasetlist']] %<>% c(dataset,.) %>% unique

    ## adding command to ensure new data is in the datasetlist
    ncmd <- paste0("\n## register the new dataset\nr_data[[\"datasetlist\"]] <- c(\"", dataset, "\", r_data[[\"datasetlist\"]]) %>% unique\n")
    if (!is_empty(r_data[[paste0(input$dataset,"_descr")]]))
      ncmd %<>% paste0("\nr_data[[\"",paste0(dataset,"_descr"),"\"]] <- r_data[[\"", paste0(input$dataset,"_descr"),"\"]]")
	}

  if (input$tr_change_type == 'remove_na') {
    cmd <- .remove_na(input$dataset, vars = input$tr_vars, input$tr_dataset, nr_col = ncol(dat))
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == 'remove_dup') {
    cmd <- .remove_dup(input$dataset, vars = input$tr_vars, input$tr_dataset, nr_col = ncol(dat))
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == 'show_dup') {
    cmd <- .show_dup(input$dataset, vars = input$tr_vars, input$tr_dataset, nr_col = ncol(dat))
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == 'holdout') {
    cmd <- .holdout(input$dataset, vars = input$tr_vars, filt = input$data_filter, rev = input$tr_holdout_rev, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == 'tab2dat') {
    cmd <- .tab2dat(input$dataset, input$tr_tab2dat, vars = input$tr_vars, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == 'reorg_vars') {
    cmd <- .reorg_vars(input$dataset, vars = input$tr_reorg_vars, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == 'type') {
    cmd <- .change_type(input$dataset, fun = input$tr_typefunction, vars = input$tr_vars, ext = input$tr_typename, input$tr_dataset)
  	r_data[[dataset]][,colnames(dat)] <- dat
  } else if (input$tr_change_type == 'transform') {
    cmd <- .transform(input$dataset, fun = input$tr_transfunction, vars = input$tr_vars, ext = input$tr_ext, input$tr_dataset)
    r_data[[dataset]][,colnames(dat)] <- dat
  } else if (input$tr_change_type == 'training') {
    cmd <- .training(input$dataset, n = input$tr_training_n, nr = nrow(dat), name = input$tr_training, input$tr_dataset)
    r_data[[dataset]][,colnames(dat)] <- dat
  } else if (input$tr_change_type == 'normalize') {
    cmd <- .normalize(input$dataset, vars = input$tr_vars, nzvar = input$tr_normalizer, ext = input$tr_ext_nz, input$tr_dataset)
    r_data[[dataset]][,colnames(dat)] <- dat
  } else if (input$tr_change_type == 'bin') {
    cmd <- .bin(input$dataset, vars = input$tr_vars, bins = input$tr_bin_n, rev = input$tr_bin_rev, ext = input$tr_ext_bin, input$tr_dataset)
    r_data[[dataset]][,colnames(dat)] <- dat
  } else if (input$tr_change_type == 'reorg_levs') {
    cmd <- .reorg_levs(input$dataset, input$tr_vars[1], input$tr_reorg_levs, input$tr_roname, input$tr_dataset)
    r_data[[dataset]][,colnames(dat)] <- dat
  } else if (input$tr_change_type == 'recode') {
    cmd <- .recode(input$dataset, input$tr_vars[1], input$tr_recode, input$tr_rcname, input$tr_dataset)
    r_data[[dataset]][,colnames(dat)] <- dat
	} else if (input$tr_change_type == 'rename') {
    cmd <- .rename(input$dataset, input$tr_vars, input$tr_rename, input$tr_dataset)
    r_data[[dataset]] %<>% rename_(.dots = setNames(input$tr_vars, colnames(dat)))
  } else if (input$tr_change_type == 'create') {
    cmd <- .create(input$dataset, cmd = input$tr_create, byvar = input$tr_vars, input$tr_dataset)
    r_data[[dataset]][,colnames(dat)] <- dat
	} else if (input$tr_change_type == 'replace') {
    cmd <- .replace(input$dataset, input$tr_vars, input$tr_replace, input$tr_dataset)
  	r_data[[dataset]][,colnames(dat)] <- dat
  	r_data[[dataset]][, input$tr_replace] <- list(NULL)
  } else if (input$tr_change_type == 'clip') {
    cmd <- paste0("## using the clipboard for data transformation is not reproducible - no command generated\n")
    r_data[[dataset]][,colnames(dat)] <- dat
  }

  ## update the command log
  updateTextInput(session, "tr_log", value = paste0(input$tr_log, "\n", paste0(cmd,ncmd)))

	## reset input values once the changes have been applied
	updateSelectInput(session = session, inputId = "tr_change_type", selected = "none")
	updateSelectInput(session = session, inputId = "dataset", select = dataset)
})

observeEvent(input$tr_change_type, {
	## reset all values when tr_change_type is changed
	updateTextInput(session = session, inputId = "tr_create", value = "")
 	updateTextInput(session = session, inputId = "tr_recode", value = "")
 	updateTextInput(session = session, inputId = "tr_rename", value = "")
 	updateTextInput(session = session, inputId = "tr_paste", value = "")
	updateSelectInput(session = session, inputId = "tr_typefunction", selected = "none")
	updateSelectInput(session = session, inputId = "tr_transfunction", selected = "none")
  updateSelectInput(session = session, inputId = "tr_replace", selected = "None")
  updateSelectInput(session = session, inputId = "tr_normalizer", selected = "none")
})

observeEvent(input$transform_report, {
  cmd <- paste0("```{r}\n", input$tr_log,"\n```\n")
  updateTextInput(session, "tr_log", value = "")
  update_report_fun(cmd)
})
