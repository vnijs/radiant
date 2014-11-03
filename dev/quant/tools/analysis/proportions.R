###############################
# Compare proportions
###############################

# output$uiCp_var1 <- renderUI({
#  	varCls <- getdata_class()
# 	isFct <- "factor" == varCls
#   vars <- varnames()[isFct]
#   if(length(vars) == 0) return()
#   selectInput(inputId = "cp_var1", label = "Select a grouping factor:", choices = vars,
#   	selected = names(vars[vars == values$cp_var1]), multiple = FALSE)
# })
#
# output$uiCp_var2 <- renderUI({
#
# 	if(is.null(input$cp_var1)) return()
#   varCls <- getdata_class()
# 	isFct <- "factor" == varCls
#   vars <- varnames()[isFct]
# 	if(!input$cp_var1 %in% vars) return()
# 	vars <- vars[-which(vars == input$cp_var1)]
#   if(length(vars) == 0) return()
#   selectInput(inputId = "cp_var2", label = "Select a 2-level factor:", choices = vars,
#   	selected = names(vars[vars %in% values$cp_var2]), multiple = FALSE)
# })
#
# ui_compareProps <- function() {
#   list(wellPanel(
#     uiOutput("uiCp_var1"),
#     uiOutput("uiCp_var2")
# 		# alternative and siglevel ignored per the helpfile
#     # conditionalPanel(condition = "input.analysistabs == 'Summary'",
#     #   selectInput(inputId = "cp_alternative", label = "Alternative hypothesis", choices = alt,
#     #   	selected = values$cp_alternative),
#     #   sliderInput('cp_sigLevel',"Significance level:", min = 0.85, max = 0.99,
#     #   	value = values$cp_sigLevel, step = 0.01)
#     # )
#     ),
# 	 	helpModal('Compare proportions','compareProps',includeHTML("tools/help/compareProps.html"))
#  	)
# }
#
# summary.compareProps <- function(result) {
# 	print(result$test)
# 	prop.table(result$tab, 1)
# }
#
# plot.compareProps <- function(result) {
#
# 	dat <- na.omit( getdata()[,c(input$cp_var1,input$cp_var2)] )
# 	p <- ggplot(dat, aes_string(x = input$cp_var1, fill = input$cp_var2)) + geom_bar(alpha=.3, position = "fill") +
# 				labs(list(title = paste("Comparing proportions of ",input$cp_var2,"$",levels(dat[,1])[1], " across levels of ",input$cp_var1, sep = ""),
# 					x = paste("Factor levels for ", input$cp_var1), y = "Count", fill = input$cp_var2))
#
# 	print(p)
# }
#
# compareProps <- reactive({
#
# 	ret_text <- "This analysis requires variables of type factor.\nPlease select another dataset."
# 	if(is.null(input$cp_var1) || is.null(input$cp_var2)) return(ret_text)
# 	if(is.null(inChecker(c(input$cp_var1, input$cp_var2)))) return(ret_text)
#
# 	var1 <- input$cp_var1
# 	var2 <- input$cp_var2
#
# 	dat <- na.omit( getdata()[,c(var1,var2)] )
# 	lev1 <- levels(dat[,1])
# 	lev2 <- levels(dat[,2])
# 	if(length(lev2) >2) return("The selected variable has more than two levels.\nTry another variable or a cross-tab.")
#
# 	tab <- table(group = dat[,input$cp_var1], variable = dat[,input$cp_var2])
#
# 	# tab <- table(group = facebook$more.important, variable = facebook$gender)
# 	# prop.test(tab, correct = FALSE, alternative = 'less', conf.level = .95)
#
# 	# alternative and siglevel ignored per the helpfile
# 	# pt <- prop.test(tab, correct = FALSE, alternative = input$cp_alternative,
# 	# 	conf.level = input$cp_sigLevel)
#
# 	pt <- prop.test(tab, correct = FALSE)
#
# 	pt$data.name <- paste("Group = ",var1,", variable = ",var2, " (level ", levels(dat[,var2])[1],")",sep = "")
# 	names(pt$estimate) <-  paste(paste("P(",var2,"$",lev2[1],")|",var1, sep = ""),"$",lev1, sep = "")
# 	list('test' = pt, 'table' = tab)
# })
