# glm_link <- c("Logit" = "logit", "Probit" = "probit")
# glm_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
# glm_predict <- c("None" = "", "Data" = "data","Command" = "cmd")
# glm_check <- c("VIF" = "vif", "Confidence intervals" = "confint",
#                "Standardized coefficients" = "standardize",
#                "Stepwise selection" = "stepwise")

# glm_int_vec <- reg_int_vec

# # list of function arguments
# glm_args <- as.list(formals(glm_reg))

# # list of function inputs selected by user
# glm_inputs <- reactive({
#   # loop needed because reactive values don't allow single bracket indexing
#   for(i in names(glm_args))
#     glm_args[[i]] <- input[[i]]
#   if(!input$show_filter) glm_args$data_filter = ""
#   glm_args
# })


# # ################################################################
# # # Generalized Linear Models
# # ################################################################

# output$uiGlm_var1 <- renderUI({
# 	isFct <- "factor" == getdata_class()
#  	vars <- varnames()[isFct]
#  	if(length(vars) == 0) return()
#   selectInput(inputId = "glm_var1", label = "Dependent variable:", choices = vars,
#    	selected = names(vars[vars == values$glm_var1]), multiple = FALSE)
# })

# output$uiGlm_var2 <- renderUI({
# 	vars <- varnames()
#  	vars <- vars[-which(vars == input$glm_var1)]
#   if(length(vars) == 0) return()
#   selectInput(inputId = "glm_var2", label = "Independent variables:", choices = vars,
#    	selected = names(vars[vars %in% values$glm_var2]), multiple = TRUE)
# })

# output$uiGlm_var3 <- renderUI({
#   vars <- input$glm_var2
#   if(is.null(vars)) return()
# 	if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)
#   selectInput(inputId = "glm_var3", label = "Variables to test:", choices = vars,
#    	selected = values$glm_var2, multiple = TRUE)
# })

# output$uiGlm_intsel <- renderUI({
#   vars <- input$glm_var2
#   if(is.null(vars) || length(vars) < 2) return()
#  	if(vars %in% varnames()) vars <- glm_int_vec(vars,input$glm_interactions)

# 	selectInput("glm_intsel", label = "", choices = vars,
#    	selected = values$glm_intsel, multiple = TRUE)
# })

# ui_glmreg <- function() {
#   list(wellPanel(
#     radioButtons(inputId = "glm_linkfunc", label = "", glm_linkfunc,
#     	selected = values$glm_linkfunc),
#     # radioButtons(inputId = "glm_glmtype", label = "", c("GLM" = "glm", "Bayes GLM" = "bayesglm"), selected = "GLM"),
#     uiOutput("uiGlm_var1"),
#     uiOutput("uiGlm_var2"),
#  	  checkboxInput(inputId = "glm_standardize", label = "Standardized coefficients",
#  	  	value = values$glm_standardize),
#     radioButtons(inputId = "glm_interactions", label = "Interactions:", glm_interactions,
#     	selected = values$glm_interactions),
#     conditionalPanel(condition = "input.glm_interactions != 'none'",
#   		uiOutput("uiGlm_intsel")
#   	),
#     conditionalPanel(condition = "input.analysistabs == 'Summary'",
# 	    uiOutput("uiGlm_var3")
#   	),
#     conditionalPanel(condition = "input.analysistabs == 'Plots'",
#       selectInput("glm_plots", "Plots:", choices = g_plots,
#       	selected = values$glm_plots, multiple = FALSE)
#     actionButton("saveglmres", "Save residuals")
#   	),
# 		helpModal('Generalized Linear Models (GLM)','glmreg',includeHTML("tools/help/glmreg.html"))
# 	)
# }

# # # analysis reactive
# # .glmreg <- reactive({

# # 	ret_text <- "This analysis requires a dependent variable of type factor\nand one or more independent variables.\nPlease select another dataset."
# # 	if(is.null(input$glm_var1)) return(ret_text)
# # 	vars <- input$glm_var2
# # 	if(is.null(vars)) return("Please select one or more independent variables.")
# # 	if(is.null(inChecker(c(input$glm_var1, vars)))) return(ret_text)

# # 		# call function

# # })

# observe({
#   if(input$save_glm_res %>% not_pressed) return()
#   isolate({
#     result <- .glm()
#     if(result %>% is.character) return()
#     result$mod$residuals %>% data.frame %>% changedata('residuals')
#   })
# })

# # # save residuals
# # observe({
# # 	if(is.null(input$saveglmres) || input$saveglmres == 0) return()
# # 	isolate({
# # 		if(is.character(glmreg())) return()
# # 		changedata(data.frame(residuals(glmreg())), "residuals")
# #  	})
# # })
