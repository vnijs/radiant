#############################################
# View table output of the selected dataset
#############################################
output$ui_view_vars <- renderUI({
  vars <- varnames()
  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_multiple("view_vars",vars, vars), multiple = TRUE,
    selectize = FALSE, size = min(15, length(vars)))
})

output$ui_View <- renderUI({
  list(
    wellPanel(
      uiOutput("ui_view_vars")
    ),
    help_modal('View','view_help',inclMD(file.path(r_path,"base/tools/help/view.md")))
  )
})

shinyFun = function(name) getFromNamespace(name, 'shiny')

my_dataTablesJSON = function(data, req) {
  n = nrow(data)
  # DataTables requests were sent via POST
  URLdecode = shinyFun('URLdecode')
  params = URLdecode(rawToChar(req$rook.input$read()))
  q = shiny::parseQueryString(params, nested = TRUE)
  ci = q$search[['caseInsensitive']] == 'true'

  # global searching
  i = seq_len(n)
  if (q$search[['value']] != '') {
    i0 = apply(data, 2, function(x) {
      grep2(q$search[['value']], as.character(x),
            fixed = q$search[['regex']] == 'false', ignore.case = ci)
    })
    i = intersect(i, unique(unlist(i0)))
  }

  # search by columns
  if (length(i)) for (j in names(q$columns)) {
    col = q$columns[[j]]
    # if the j-th column is not searchable or the search string is "", skip it
    if (col[['searchable']] != 'true') next
    if ((k <- col[['search']][['value']]) == '') next
    j = as.integer(j)
    dj = data[, j + 1]
    ij = if (is.numeric(dj) || lubridate::is.Date(dj)) {
      r = commaToRange(k)
      if (length(r) != 2)
        stop('The range of a numeric / date / time column must be of length 2')
      if (lubridate::is.Date(dj)) {
        # r is milliseconds
        r = as.POSIXct(r / 1000, origin = '1970-01-01')
        if (inherits(dj, 'Date')) r = as.Date(r)
      }
      which(dj >= r[1] & dj <= r[2])
    } else if (is.factor(dj)) {
      which(dj %in% jsonlite::fromJSON(k))
    } else {
      grep2(k, as.character(dj), fixed = col[['search']][['regex']] == 'false',
            ignore.case = ci)
    }
    i = intersect(ij, i)
    if (length(i) == 0) break
  }
  if (length(i) != n) data = data[i, , drop = FALSE]

  # sorting
  oList = list()
  for (ord in q$order) {
    k = ord[['column']]  # which column to sort
    d = ord[['dir']]     # direction asc/desc
    if (q$columns[[k]][['orderable']] != 'true') next
    col = data[, as.integer(k) + 1]
    oList[[length(oList) + 1]] = (if (d == 'asc') identity else `-`)(
      if (is.numeric(col)) col else xtfrm(col)
    )
  }
  if (length(oList)) {
    i = do.call(order, oList)
    data = data[i, , drop = FALSE]
  }
  # paging
  if (q$length != '-1') {
    i = seq(as.integer(q$start) + 1L, length.out = as.integer(q$length))
    i = i[i <= nrow(data)]
    fdata = data[i, , drop = FALSE]  # filtered data
  } else fdata = data

  fdata = unname(as.matrix(fdata))
  if (is.character(fdata) && q$escape != 'false') {
    if (q$escape == 'true') fdata = htmltools::htmlEscape(fdata) else {
      k = as.integer(strsplit(q$escape, ',')[[1]])
      # use seq_len() in case escape = negative indices, e.g. c(-1, -5)
      for (j in seq_len(ncol(fdata))[k]) fdata[, j] = htmltools::htmlEscape(fdata[, j])
    }
  }

  toJSON = shinyFun('toJSON')
  res = toJSON(list(
    draw = as.integer(q$draw),
    recordsTotal = n,
    recordsFiltered = nrow(data),
    data = fdata
  ))
  httpResponse = shinyFun('httpResponse')
  # TODO: enc2utf8() may not be necessary if we use jsonlite
  httpResponse(200, 'application/json', enc2utf8(res))
}

# when both ignore.case and fixed are TRUE, we use grep(ignore.case = FALSE,
# fixed = TRUE) to do lower-case matching of pattern on x
grep2 = function(pattern, x, ignore.case = FALSE, fixed = FALSE, ...) {
  if (fixed && ignore.case) {
    pattern = tolower(pattern)
    x = tolower(x)
    ignore.case = FALSE
  }
  # when the user types in the search box, the regular expression may not be
  # complete before it is sent to the server, in which case we do not search
  if (!fixed && inherits(try(grep(pattern, ''), silent = TRUE), 'try-error'))
    return(seq_along(x))
  grep(pattern, x, ignore.case = ignore.case, fixed = fixed, ...)
}

# convert a string of the form "lower,upper" to c(lower, upper)
commaToRange = function(string) {
  if (!grepl(',', string)) return()
  r = strsplit(string, ',')[[1]]
  if (length(r) > 2) return()
  if (length(r) == 1) r = c(r, '')  # lower,
  r = as.numeric(r)
  if (is.na(r[1])) r[1] = -Inf
  if (is.na(r[2])) r[2] = Inf
  r
}

output$dataviewer <- DT::renderDataTable({

  if (not_available(input$view_vars)) return()

  dat <- select_(.getdata(), .dots = input$view_vars)
  # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
  action = DT::dataTableAjax(session, dat, rownames = FALSE)

  DT::datatable(dat, filter = "top", rownames = FALSE, server = TRUE,
    # class = "compact",
    options = list(
      ajax = list(url = action),
      search = list(regex = TRUE),
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      autoWidth = TRUE,
      processing = FALSE,
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
    )
  )
})

# output$ui_view_vars_old <- renderUI({
#   vars <- varnames()
#   selectInput("view_vars_old", "Select variables to show:", choices  = vars,
#     selected = state_multiple("view_vars_old",vars, vars), multiple = TRUE,
#     selectize = FALSE, size = min(15, length(vars)))
# })
#
# output$ui_View_old <- renderUI({
#   list(
#     wellPanel(
#       uiOutput("ui_view_vars_old")
#     ),
#     help_modal('View','view_help',inclMD(file.path(r_path,"base/tools/help/view.md")))
#   )
# })
#
#
# output$dataviewer_old <- shiny::renderDataTable({
#
#   if (not_available(input$view_vars_old)) return()
#   select_(.getdata(), .dots = input$view_vars_old)
#
# }, options = list(orderClasses = TRUE, caseInsensitive = TRUE,
#                   lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
#                   pageLength = 10, search = list(regex = TRUE)))
