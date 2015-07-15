############################################
## Pivot - combination of Explore and View
############################################

# expl_args <- as.list(formals(explore))

# ## list of function inputs selected by user
# expl_inputs <- reactive({
#   ## loop needed because reactive values don't allow single bracket indexing
#   expl_args$data_filter <- if (input$show_filter) input$data_filter else ""
#   expl_args$dataset <- input$dataset
#   for (i in r_drop(names(expl_args)))
#     expl_args[[i]] <- input[[paste0("expl_",i)]]

#   if (is_empty(input$expl_byvar)) expl_args$fun <- c("length", "mean_rm")

#   expl_args
# })


# # UI-elements for explore
# output$ui_pvt_cvars <- renderUI({
#   vars <- groupable_vars()
#   selectizeInput("pvt_cvars", label = "Categorical variables:", choices = vars,
#     selected = state_multiple("pvt_cvars",vars, ""), multiple = TRUE,
#     options = list(placeholder = 'Select categorical variables',
#                    plugins = list('remove_button', 'drag_drop'))
#   )
# })

# output$ui_pvt_nvars <- renderUI({
#   isNum <- "numeric" == .getclass() | "integer" == .getclass()
#   vars <- varnames()[isNum]
#   selectInput("pvt_vars", label = "Select variable:", choices = vars,
#     selected = state_single("pvt_vars",vars), multiple = FALSE)
# })


# output$ui_expl_fun <- renderUI({
#   if (is_empty(input$expl_byvar)) return()

#   sel <- if(is_empty(input$expl_fun)) state_multiple("expl_fun", expl_functions, c("length","mean_rm"))
#          else input$expl_fun

#   selectizeInput("expl_fun", label = "Apply function(s):",
#                  choices = expl_functions,
#                  # selected = state_multiple("expl_fun", expl_functions, c("length","mean_rm")),
#                  selected = sel,
#                  multiple = TRUE,
#                  options = list(placeholder = 'Select functions',
#                                 plugins = list('remove_button', 'drag_drop'))
#     )
# })

# output$ui_expl_viz <- renderUI({
#   if (is_empty(input$expl_byvar)) return()
#   checkboxInput('expl_viz', 'Show plot',
#                 value = state_init("expl_viz", FALSE))
# })

# output$ui_Pivot <- renderUI({
#   list(
#     wellPanel(
#       uiOutput("ui_pvt_cvars"),
#       uiOutput("ui_pvt_nvars")
#     ),
#     help_and_report(modal_title = "Pivot",
#                     fun_name = "pivotr",
#                     help_file = inclMD(file.path(r_path,"base/tools/help/pivot.md")))
#   )
# })

# .explore <- reactive({
#   if (not_available(input$expl_vars)) return()
#   withProgress(message = 'Calculating', value = 0, {
#     do.call(explore, expl_inputs())
#   })
# })

# output$expl_summary <- renderPrint({
#   if (!is.null(input$expl_tab) && input$expl_tab)
#     .explore() %>% { if (is.null(.)) invisible() else summary(.) }
# })

# expl_plot_width <- function() 650
# expl_plot_height <- function()
#   400 * length(input$expl_fun) * length(input$expl_vars)

# output$expl_plots <- renderPlot({
#   if (!input$expl_viz || is.null(input$expl_byvar)) return()
#   withProgress(message = 'Making plot', value = 0, {
#     .explore() %>% { if (is.null(.)) invisible() else print(plot(., shiny = TRUE)) }
#   })
# }, width = expl_plot_width, height = expl_plot_height)

# observe({
#   if (not_pressed(input$explore_report)) return()
#   isolate({
#     if (!is_empty(input$expl_byvar) && input$expl_viz == TRUE) {
#       inp_out <- list("","")
#       outputs <- c("summary","plot")
#       figs <- TRUE
#     } else {
#       outputs <- c("summary")
#       inp_out <- list("","")
#       figs <- FALSE
#     }
#     update_report(inp_main = clean_args(expl_inputs(), expl_args),
#                   fun_name = "explore",
#                   inp_out = inp_out,
#                   outputs = outputs,
#                   figs = figs,
#                   fig.width = round(7 * expl_plot_width()/650,2),
#                   fig.height = round(7 * expl_plot_height()/650,2))
#   })
# })



# #############################################
# # View table output of the selected dataset
# #############################################
# output$ui_view_vars <- renderUI({
#   vars <- varnames()
#   selectInput("view_vars", "Select variables to show:", choices  = vars,
#     selected = state_multiple("view_vars",vars, vars), multiple = TRUE,
#     selectize = FALSE, size = min(15, length(vars)))
# })

# output$ui_View <- renderUI({
#   list(
#     wellPanel(
#       uiOutput("ui_view_vars")
#     ),
#     help_modal('View','view_help',inclMD(file.path(r_path,"base/tools/help/view.md")))
#   )
# })

# my_dataTablesFilter = function(data, req) {
#   ## to implement
# }

# output$pivotr <- DT::renderDataTable({

#   # if (not_available(input$view_vars)) return()
#   # dat <- select_(.getdata(), .dots = input$view_vars)
#   dat <- mtcars

#   # action = DT::dataTableAjax(session, dat, rownames = FALSE, filter = my_dataTablesFilter)
#   DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
#     rownames = FALSE, style = "bootstrap", escape = FALSE,
#     # extensions = 'KeyTable',
#     options = list(
#       # search = list(search = rnorm(1)), order = list(list(2, 'asc'), list(1, 'desc')),
#       # stateSave = TRUE,   # maintains but does not show column filter settings
#       search = list(regex = TRUE),
#       autoWidth = TRUE,
#       columnDefs = list(list(className = 'dt-center', targets = "_all")),
#       processing = FALSE,
#       pageLength = 10,
#       lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
#     )
#   )
# })

# # search = list(search = 'Ma'), order = list(list(2, 'asc'), list(1, 'desc'))
# # output$tbl_state = renderPrint(str(input$dataviewer_state))
