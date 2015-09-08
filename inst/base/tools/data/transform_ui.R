# UI-elements for transform
output$ui_tr_vars <- renderUI({
  vars <- varnames()
  if (not_available(vars)) return()
  selectInput("tr_vars", "Select variable(s):", choices  = vars,
    # selected = state_multiple("tr_vars", vars),
    # selected = NULL,
    multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
})

output$ui_tr_replace <- renderUI({
  vars <- varnames()
  selectInput("tr_replace", "Select replacement variables:", choices  = vars,
    # selected = state_multiple("tr_replace", vars),
    # selected = NULL,
    multiple = TRUE, size = min(2, length(vars)), selectize = FALSE)
})

output$ui_tr_reorg_vars <- renderUI({
  vars <- varnames()
  selectizeInput("tr_reorg_vars", "Reorder/remove variables:", choices  = vars,
    selected = vars, multiple = TRUE,
    options = list(placeholder = 'Select variable(s)',
                   plugins = list('remove_button', 'drag_drop')))
})

output$ui_tr_normalizer <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- varnames()[isNum]
  if (length(vars) == 0) return(NULL)
  selectInput("tr_normalizer", "Normalizing variable:", c("None" = "none", vars),
              selected = "none")
})

output$ui_tr_reorg_levs <- renderUI({
	if (not_available(input$tr_vars)) return()
  fctCol <- input$tr_vars[1]
	isFct <- "factor" == .getclass()[fctCol]
  if (!isFct) return()
	levs <- .getdata()[[fctCol]] %>% levels

  selectizeInput("tr_reorg_levs", "Reorder/remove levels:", choices  = levs,
    selected = levs, multiple = TRUE,
    options = list(placeholder = 'Select level(s)',
                   plugins = list('remove_button', 'drag_drop')))
})

output$ui_tr_log <- renderUI({
  tagList(
    "<label>Transform command log:</label><br>" %>% HTML,
     shinyAce::aceEditor("tr_log", mode = "r",
      # height="auto",
      height="135px",
      value = state_init("tr_log",""))
    # tags$textarea(class="form-control", id="tr_log",
    #               rows="8", style="max-width: 800px;",
    #               state_init("tr_log", ""))
  )
})


ext_options <- list("none" = "", "log" = "_log", "exp" = "_exp",
                    "square" = "_sq", "sqrt" = "_sqrt", "center" = "_ct",
                    "standardize" = "_st", "inverse" = "_inv",
                    "median_split" = "_mds", "decile_split" = "_ds")

output$ui_tr_ext <- renderUI({
  trfun <- input$tr_transfunction
  if (is_empty(trfun)) trfun <- "none"
  ext <- ext_options[[trfun]]
  # textInput("tr_ext", "Variable name extension:", ext)
  returnTextInput("tr_ext", "Variable name extension:", ext)
})

output$ui_tr_ext_nz <- renderUI({
  if (is_empty(input$tr_normalizer, "none")) return()
  ext <- paste0("_", input$tr_normalizer)
  # textInput("tr_ext_nz", "Variable name extension:", ext)
  returnTextInput("tr_ext_nz", "Variable name extension:", ext)
})

output$ui_tr_rcname <- renderUI({
  if (is_empty(input$tr_vars)) return()
  rcname <- paste0(input$tr_vars, "_rc")
  returnTextInput("tr_rcname", "Recoded variable name:", rcname)
})


trans_options <- list("None" = "none", "Log" = "log", "Exp" = "exp",
                      "Square" = "square", "Square-root" = "sqrt",
                      "Center" = "center", "Standardize" = "standardize", "Inverse" = "inverse",
                      "Median split" = "median_split", "Deciles" = "decile_split")

type_options <- list("None" = "none", "As factor" = "as_factor",
                     "As numeric" = "as_numeric", "As integer" = "as_integer",
                     "As character" = "as_character", "As date (mdy)" = "as_mdy",
                     "As date (dmy)" = "as_dmy", "As date (ymd)" = "as_ymd",
                     "As date/time (ymd_hms)" = "as_ymd_hms",
                     "As time (hm)" = "as_hm", "As time (hms)" = "as_hms")

trans_types <- list("None" = "none", "Type" = "type", "Transform" = "transform",
                    "Normalize" = "normalize", "Create" = "create",
                    "Training sample" = "training",
                    "Clipboard" = "clip", "Recode" = "recode",
                    "Rename" = "rename", "Replace" = "replace",
                    "Reorder/remove variables" = "reorg_vars",
                    "Reorder/remove levels" = "reorg_levs",
                    "Remove missing" = "remove_na")

output$ui_Transform <- renderUI({
	## Inspired by Ian Fellow's transform ui in JGR/Deducer
  tagList(wellPanel(
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
    conditionalPanel(condition = "input.tr_change_type == 'create'",
	    returnTextAreaInput("tr_create", "Create (e.g., x = y - z):", "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'training'",
      tags$table(
        tags$td(numericInput("tr_training_n", label = "Size:", min = 0, value = .7)),
                # value = state_init("tr_training_n", .9))),
        tags$td(textInput("tr_training", "Variable name:", "training"))
                # state_init("tr_training","training")))
      )
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
    conditionalPanel(condition = "input.tr_change_type != 'none'",
	    tags$table(
	      tags$td(textInput("tr_dataset", "Store changes in:", input$dataset)),
	      tags$td(actionButton("tr_store", "Store"), style="padding-top:30px;")
	    )
	  )),
    help_and_report(modal_title = "Transform",
                    fun_name = "transform",
                    help_file = inclMD(file.path(r_path, "base/tools/help/transform.md")))
		# help_modal("Transform", "transform_help",
		#   inclMD(file.path(r_path,"base/tools/help/transform.md")))

	)
})

reorg_vars <- function(dataset,
                       vars = "",
                       store_dat = "",
                       store = TRUE) {

  if (!store || !is.character(dataset))
    return(getdata(dataset, vars, na.rm = FALSE))

  if (store_dat == "") store_dat <- dataset
  mess <- if (all(vars == "")) ""
          else paste0("\nReorganized variables: ", paste0(vars, collapse=", "), " (", lubridate::now(), ")")

  ## creating command for log-input
  cmd <- paste0("\n## reorder/remove variables\nr_data[[\"",store_dat,"\"]] <- select(r_data[[\"",dataset,"\"]], ", paste0(vars, collapse = ", "),")\n")
  update_log(cmd, input$tr_log)
}

remove_na <- function(dataset,
                      vars = "",
                      store_dat = "",
                      store = TRUE) {

  if (!store || !is.character(dataset))
    return(filter_na(dataset, vars))

  if (store_dat == "") store_dat <- dataset

  mess <- if (all(vars == "")) {
            paste0("\nRemoved missing values on ", lubridate::now())
          } else {
            paste0("\nRemoved missing values using: ", paste0(vars, collapse=", "), " (", lubridate::now(), ")")
          }

  ## creating command for log-input
  if (all(vars == "")) {
    cmd <- paste0("\n## remove missing values\nr_data[[\"",store_dat,"\"]] <- filter_na(r_data[[\"",dataset,"\"]])\n")
  } else {
    cmd <- paste0("\n## remove missing values\nr_data[[\"",store_dat,"\"]] <- filter_na(r_data[[\"",dataset,"\"]], vars = c(\"", paste0(vars, collapse = "\",\""),"\"))")
  }

  # updateTextInput(session = session, inputId = "tr_log", value = paste0(input$tr_log, cmd))
  update_log(cmd, input$tr_log)
}


change_type <- function(dataset, fun,
                        vars = "",
                        store_dat = "",
                        store = TRUE) {

  if (!store || !is.character(dataset)) {
    fun <- get(fun)
    return(mutate_each_(dataset, funs(fun), vars))
  }

  ## creating command for log-input
  ## changes are always in-place
  cmd <- paste0("\n## change variable type\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(", fun, "), ", paste0(vars, collapse = ", "),")\n")
  update_log(cmd, input$tr_log)
}

transform_vars <- function(dataset, fun,
                           vars = "",
                           ext = "",
                           store_dat = "",
                           store = TRUE) {

  if (!store && !is.character(dataset)) {
    # print(fun)
    fun <- get(fun)
    if (ext == "") {
      return(mutate_each_(dataset, funs(fun), vars))
    } else {
      return(mutate_each_(dataset, funs(fun), vars) %>% set_colnames(paste0(vars, ext)))
    }
  }

  ## creating command for log-input
  if (ext == "")
    cmd <- paste0("\n## transform variable\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(",fun, "), ", paste0(vars, collapse = ", "), ")")
  else
    cmd <- paste0("\n## transform variable\nr_data[[\"",store_dat,"\"]] <- mutate_each_add(r_data[[\"",dataset,"\"]], fun = \"", fun, "\", ext = \"", ext, "\", ", paste0(vars, collapse = ", "), ")")

  update_log(cmd, input$tr_log)
}

make_train_tr <- function(dataset,
                          n = .7,
                          nr = 100,
                          name = "training",
                          store_dat = "",
                          store = TRUE) {

  if (is_empty(name)) name <- "training"
  if (!store && !is.character(dataset)) {
    ## create training variable
    n <- n %>% {ifelse (. < 0 || is.na(.) || . > nr, .7, .)}
    return(data.frame(make_train(n, nr)) %>% setNames(name))
  }
  # n <- .8
  # nr <- 100
  # name <- "training"

  ## creating command for log-input
  # cmd <- paste0("\n## created variable to select training sample\nr_data[[\"",dataset,"\"]][[\"", name, "\"]] <- make_train(", n, ", ", nr, ")\n")
  cmd <- paste0("\n## created variable to select training sample\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", name, " = make_train(", n, ", n()))\n")
  update_log(cmd, input$tr_log)

}

normalize_tr <- function(dataset, vars, nzvar,
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
    dat <- select_(dat, .dots = vars) / nz[[1]]
    colnames(dat) <- paste0(vars, ext)
    return(dat)
  }

  if (ext == "")
    cmd <- paste0("\n## normalize variables (overwriting existing variables)\nr_data[[\"",store_dat,"\"]] <- mutate_each(r_data[[\"",dataset,"\"]], funs(./",nzvar,"), ext = \"", ext, "\", ", paste0(vars, collapse = ", "), ")")
  else
    cmd <- paste0("\n## normalize variables\nr_data[[\"",store_dat,"\"]] <- normalize_each_add(r_data[[\"",dataset,"\"]], ext = \"", ext, "\", ", nzvar, ", ", paste0(vars, collapse = ", "), ")")

  update_log(cmd, input$tr_log)
}

reorg_levs <- function(dataset, fct, levs,
                       store_dat = "",
                       store = TRUE) {

  if (!store || !is.character(dataset)) {
      # return(select_(dataset, fct) %>% mutate_each_(funs(factor(., levels = levs)), vars = fct))
      return(data.frame(factor(dataset[[fct]], levels = levs)) %>% setNames(fct))
  }

  ## creating command for log-input
  cmd <- paste0("\n## change factor levels\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", fct, " = factor(", fct, ", levels = c(\"", paste0(levs, collapse = "\",\""), "\")))\n")
  update_log(cmd, input$tr_log)
}

recode_tr <- function(dataset, var, cmd,
                      rcname = "",
                      store_dat = "",
                      store = TRUE) {

  cmd <- cmd %>% gsub("\\s","", .) %>% gsub("\"","\'",.) %>% gsub(",",";", .)
  if (is_empty(rcname)) rcname <- paste0(var, "_rc")
  if (!store || !is.character(dataset)) {
    if (cmd == "") return(dataset)
    # nvar <- try(do.call(car::recode, list(dataset[[var]], cmd)), silent = TRUE)
    nvar <- try(car::recode(dataset[[var]], cmd), silent = TRUE)
    if (is(nvar, 'try-error')) {
      return(paste0("the recode command was not valid. the error message was:\n", attr(nvar,"condition")$message, "\nplease try again. examples are shown in the help file (click the ? icon)."))
    } else {
      return(data.frame(nvar) %>% setNames(rcname))
    }
  }

  ## creating command for log-input
  cmd <- paste0("\n## recode variable\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", rcname, " = recode(", var, ", \"", cmd, "\"))\n")
  update_log(cmd, input$tr_log)
}

rename_tr <- function(dataset, var, rnm,
                      store_dat = "",
                      store = TRUE) {

  rnm <- rnm %>% gsub("\\s","", .) %>% gsub(";",",", .)

  if (rnm != "") rnm <- unlist(strsplit(rnm, ",")) %>% .[1:min(length(.),length(var))]

  if (!store || !is.character(dataset)) {
    if (rnm[1] == "") return(dataset)
    names(dataset)[1:length(rnm)] <- rnm
    ## dplyr alternative has the same dplyr::changes result
  #   # rename_(dat, .dots = setNames(l2,l1))
    # mtcars %>% rename(a = mpg, b = cyl)
    # mtcars %>% rename(setNames(a = "mpg", b = "cyl"))
    return(dataset)
  }

  ## creating command for log-input
  cmd <- paste0("\n## rename variable(s)\nr_data[[\"",store_dat,"\"]] <- rename(r_data[[\"",dataset,"\"]], ", paste(rnm, var, sep = " = ", collapse = ", "), ")\n")
  update_log(cmd, input$tr_log)
}

create <- function(dataset, cmd,
                   store_dat = "",
                   store = TRUE) {

  # cmd <- cmd %>% gsub("\\s","", .) %>% gsub("\"","\'",.)
  cmd <- cmd %>% gsub("\"","\'",.)

  if (!store || !is.character(dataset)) {
    if (gsub("\\s","",cmd) == "") return(dataset)
    nvar <- try(do.call(within, list(dataset, parse(text = cmd))), silent = TRUE)
    if (is(nvar, 'try-error')) {
      return(paste0("The create command was not valid. The command entered was:\n\n", cmd, "\n\nThe error message was:\n\n", attr(nvar,"condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
    } else {
      nvars <- strsplit(cmd, ";")[[1]] %>% strsplit(., "=") %>% sapply("[", 1)
      return(select_(nvar, .dots = nvars))
    }
  }

  ## creating command for log-input
  cmd <- paste0("\n## create new variable(s)\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", gsub(";",",",cmd), ")\n")
  update_log(cmd, input$tr_log)
}

replace_tr <- function(dataset, var, rpl,
                       store_dat = "",
                       store = TRUE) {

  if (!store || !is.character(dataset)) {
    select_(dataset, .dots = rpl) %>% set_colnames(var)
  } else {
    ## creating command for log-input
    cmd <- paste0("\n## replace variable(s)\nr_data[[\"",store_dat,"\"]] <- mutate(r_data[[\"",dataset,"\"]], ", paste(var, rpl, sep = " = ", collapse = ", "), ") %>% select(", paste0("-",rpl, collapse = ", "),")\n")
    update_log(cmd, input$tr_log)
  }
}


inp_vars <- function(inp, rval = "")
	if (is_empty(input[[inp]])) rval else input[[inp]]

transform_main <- reactive({

	if (is.null(input$tr_change_type)) return()
  if (not_available(input$tr_vars)) {
    if (input$tr_change_type == "none")
      # return("show invisible())
      return("Select a transformation type and select one or more variables to transform.\nSelect all variables to provide an overview of the data")
    if (input$tr_change_type == "type")
      return("Select one or more variables to change their variable type")
    if (input$tr_change_type == "reorg_levs")
      return("Select a variable to type factor to change the ordering and/or number of levels")
  }

  # if (!input$tr_change_type %in% c("", "store_filtered", "create", "clip")) return()

	## don't transform when a filter is active
  selcom <- input$data_filter %>% gsub("\\s","", .) %>% gsub("\"","\'",.)
  if (!is_empty(selcom) && input$show_filter == TRUE)
  	return("A filter is active. Either uncheck the filter checkbox, remove the filter statement,\nor store the filtered data through the Data > View tab")

  ## get the active dataset
	dat <- .getdata()

  ## what data to pass on ...
	if (input$tr_change_type == "none") {
	  # if (not_available(input$tr_vars)) return(dat)
    if (not_available(input$tr_vars)) return(invisible())
 		dat <- select_(dat, .dots = input$tr_vars)
	}

  ## reorganize variables
	if (input$tr_change_type == "reorg_vars")
 	  return(reorg_vars(dat, inp_vars("tr_reorg_vars"), store = FALSE))

  ## remove missing variables
	if (input$tr_change_type == "remove_na")
		return(remove_na(dat, inp_vars("tr_vars"), store = FALSE))

  ## create training variable
  if (input$tr_change_type == 'training')
    return(make_train_tr(dat, n = input$tr_training_n, nr = nrow(dat), name = input$tr_training,  store = FALSE))

  if (input$tr_change_type == 'create')
    return(create(dat, input$tr_create, store = FALSE))

  if (input$tr_change_type == 'clip') {
    if (input$tr_paste != "") {
      cpdat <- try(read.table(header = TRUE, comment.char = "", fill = TRUE, sep = "\t", as.is = TRUE, text = input$tr_paste), silent = TRUE)
      if (is(cpdat, 'try-error')) {
        return("The copy-and-pasted data was not well formated. Please make\nsure the number of rows in the data in Radiant and in the\nspreadsheet are the same and try again.")
      } else if (nrow(cpdat) != nrow(dat)) {
        return("The copy-and-pasted data does not have the correct number of rows. Please make\nsure the number of rows in the data in Radiant and in the spreadsheet are the\nsame and try again.")
      } else {
        return(cpdat)
      }
    }
  }

  ####
  ####
  ####
  ####
  ####
  ####
  # add a function to remove duplicate rows ala filter_na
  ####
  ####
  ####
  ####
  ####


  ## only use the functions below if variables have been selected
	# if (!is.null(input$tr_vars)) {
  if (!is_empty(input$tr_vars)) {
    if (not_available(input$tr_vars)) return()
		# if (!all(input$tr_vars %in% colnames(dat))) return()

    if (!is_empty(input$tr_normalizer, "none")) {
      return(normalize_tr(dat, inp_vars("tr_vars"), input$tr_normalizer, ext = input$tr_ext_nz, store = FALSE))
    }

    if (input$tr_change_type == 'replace') {
      vars <- input$tr_vars
      rpl  <- input$tr_replace
      if (available(rpl)) {
        if (length(vars) != length(rpl))
          return(paste0("The number of replacement variables (", length(rpl), ") is not equal to the number of variables to replace (", length(vars),")"))
        return(replace_tr(dat, vars, rpl, store = FALSE))
      }
    }


		dat  <- select_(dat, .dots = input$tr_vars)
    vars <- colnames(dat)

    ## change in type is always done in-place
    if (input$tr_typefunction != "none")
      return(change_type(dat, input$tr_typefunction, inp_vars("tr_vars"), store = FALSE))

    ## change in type is always done in-place
    if (input$tr_transfunction != "none")
      return(transform_vars(dat, input$tr_transfunction, inp_vars("tr_vars"), input$tr_ext, store = FALSE))

    if (input$tr_change_type == 'reorg_levs' && !is.null(input$tr_reorg_levs)) {
        fct <- input$tr_vars[1]
        if (!is.factor(dat[[fct]]))
          return("Select a variable to type factor to change the ordering and/or number of levels")

        return(reorg_levs(dat, fct, input$tr_reorg_levs, store = FALSE))
    }

    if (input$tr_change_type ==  'recode')
      return(recode_tr(dat, inp_vars("tr_vars")[1], input$tr_recode, input$tr_rcname, store = FALSE))

    if (input$tr_change_type == 'rename') {
      if (any(input$tr_rename %in% varnames()))
        return("One or more of the new variables names already exists in the data.\nChange the specified names or use the Replace function")

      return(rename_tr(dat, inp_vars("tr_vars"), input$tr_rename, store = FALSE))
    }
	}


	dat
})

output$transform_data <- reactive({
  dat <- transform_main()
  if (is.null(dat) || is.character(dat) || ncol(dat) == 0) {
    tr_snippet()
    # return(invisible())
  } else {
    show_data_snippet(dat)
  }
})

tr_summary <- reactive({
  getsummary(.getdata())
})

tr_snippet <- reactive({
  show_data_snippet(.getdata())
})

output$transform_summary <- renderPrint({
	dat <- transform_main()
  # cf("here")
  # isolate({
    # if (is.null(dat)) return(invisible())
    if (is.null(dat) || is.character(dat)) {
      # cat(dat,"\n\n")
      cat(dat)
        # getsummary(.getdata())
        # tr_summary()
      # print(tr_summary())
    } else {
      if (is_empty(input$tr_vars) && input$tr_change_type != 'create') {
        # cat(dat)
        # tr_summary()
        tr_summary()
        # getsummary(.getdata())
      } else {
        getsummary(dat)
      }
    }
  # })
})

observeEvent(input$tr_store, {
	isolate({
		dat <- transform_main()
		if (is.null(dat)) return()
		if (is.character(dat)) return(dat)

		## saving to a new dataset if specified
		dataset <- input$tr_dataset
		if (r_data[[dataset]] %>% is.null) {
			r_data[[dataset]] <- .getdata()
			r_data[[paste0(dataset,"_descr")]] <- r_data[[paste0(input$dataset,"_descr")]]
			r_data[['datasetlist']] %<>% c(dataset,.) %>% unique
		}

    if (input$tr_change_type == 'remove_na') {
      remove_na(input$dataset, vars = input$tr_vars, input$tr_dataset)
      r_data[[dataset]] <- dat
    } else if (input$tr_change_type == 'reorg_vars') {
      reorg_vars(input$dataset, vars = input$tr_reorg_vars, input$tr_dataset)
      r_data[[dataset]] <- dat
    } else if (input$tr_change_type == 'type') {
      change_type(input$dataset, fun = input$tr_typefunction, vars = input$tr_vars, input$tr_dataset)
	  	r_data[[dataset]][,colnames(dat)] <- dat
    } else if (input$tr_change_type == 'transform') {
      transform_vars(input$dataset, fun = input$tr_transfunction, vars = input$tr_vars, ext = input$tr_ext, input$tr_dataset)
      r_data[[dataset]][,colnames(dat)] <- dat
    } else if (input$tr_change_type == 'training') {
      make_train_tr(input$dataset, n = input$tr_training_n, nr = nrow(dat), name = input$tr_training, input$tr_dataset)
      r_data[[dataset]][,colnames(dat)] <- dat
    } else if (input$tr_change_type == 'normalize') {
      normalize_tr(input$dataset, vars = input$tr_vars, nzvar = input$tr_normalizer, ext = input$tr_ext_nz, input$tr_dataset)
      r_data[[dataset]][,colnames(dat)] <- dat
    } else if (input$tr_change_type == 'reorg_levs') {
      reorg_levs(input$dataset, input$tr_vars[1], input$tr_reorg_levs, input$tr_dataset)
      r_data[[dataset]][,colnames(dat)] <- dat
    } else if (input$tr_change_type == 'recode') {
      recode_tr(input$dataset, input$tr_vars[1], input$tr_recode, input$tr_rcname, input$tr_dataset)
      r_data[[dataset]][,colnames(dat)] <- dat
		} else if (input$tr_change_type == 'rename') {
      rename_tr(input$dataset, input$tr_vars, input$tr_rename, input$tr_dataset)
  		# r_data[[dataset]] %<>% rename_(.dots = setNames(input$tr_vars, colnames(dat)))
      r_data[[dataset]][,colnames(dat)] <- dat
    } else if (input$tr_change_type == 'create') {
      create(input$dataset, input$tr_create, input$tr_dataset)
      r_data[[dataset]][,colnames(dat)] <- dat
		} else if (input$tr_change_type == 'replace') {
      replace_tr(input$dataset, input$tr_vars, input$tr_replace, input$tr_dataset)
	  	r_data[[dataset]][,colnames(dat)] <- dat
	  	r_data[[dataset]][, input$tr_replace] <- list(NULL)
	  } else {
			.changedata(dat, colnames(dat), dataset = dataset)
		}

		## reset input values once the changes have been applied
		updateSelectInput(session = session, inputId = "tr_change_type", selected = "none")

    # if (dataset != input$dataset)
		updateSelectInput(session = session, inputId = "dataset", select = dataset)
  })
})

observeEvent(input$tr_change_type, {
	## reset all values when tr_change_type is changed
	isolate({
		updateTextInput(session = session, inputId = "tr_create", value = "")
	 	updateTextInput(session = session, inputId = "tr_recode", value = "")
	 	updateTextInput(session = session, inputId = "tr_rename", value = "")
	 	updateTextInput(session = session, inputId = "tr_paste", value = "")
		updateSelectInput(session = session, inputId = "tr_typefunction", selected = "none")
		updateSelectInput(session = session, inputId = "tr_transfunction", selected = "none")
	  updateSelectInput(session = session, inputId = "tr_replace", selected = "None")
	  updateSelectInput(session = session, inputId = "tr_normalizer", selected = "none")
	})
})

observeEvent(input$transform_report, {
  isolate({
    cmd <- paste0("\n```{r}\n", input$tr_log,"\n```\n")
    # updateTextInput(session = session, inputId = "tr_log", value = "")
    shinyAce::updateAceEditor(session, "tr_log", value = "")
    update_report_fun(cmd)
  })
})

update_log <- function(cmd, log) {
  shinyAce::updateAceEditor(session, "tr_log", value = paste0(log, "\n", cmd))
}
