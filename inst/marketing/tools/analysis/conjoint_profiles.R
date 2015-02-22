###############################
# Conjoint profiles
###############################
output$ui_conjoint_profiles <- renderUI({
  list(
  	wellPanel(
			fileInput('uploadAttr', 'Upload attributes:', multiple=FALSE),
	  	downloadButton('downloadProfiles', 'Save profiles')
		),
		# helpAndReport('Conjoint profiles','conjoint_profiles',inclMD("tools/help/conjoint_profiles.md"))
  	help_and_report(modal_title = "Conjoint profiles",
  	                fun_name = "conjoint_profiles",
  	                help_file = inclMD("../marketing/tools/help/conjoint_profiles.md"))
	)
})

# output is called from the main radiant ui.R
output$conjoint_profiles <- renderUI({

		register_print_output("summary_conjoint_profiles", ".conjoint_profiles")

		cap_output_panels <- tagList(
	     tabPanel("Summary", verbatimTextOutput("summary_conjoint_profiles"))
	  )

		statTabPanel2(menu = "Conjoint",
		              tool = "Create profiles",
		              tool_ui = "ui_conjoint_profiles",
		             	output_panels = cap_output_panels)
})

.conjoint_profiles <- reactive({
	ret_text <- "Please load a file with attribute information. For an example see\nhttps://github.com/mostly-harmless/radiant/blob/master/examples/profiles-movie.txt"
	if(is.null(input$uploadAttr)) return(ret_text)
  if(is.null(r_data[['ca_attr']])) return(ret_text)

	conjoint_profiles(r_data[['ca_attr']])
})

observe({
  if(input$conjoint_profiles_report %>% not_pressed) return()
  isolate({
		update_report(inp = list(r_data[['ca_attr']]), fun_name = "conjoint_profiles",
		              outputs = "summary")
  })
})

conjoint_profiles <- function(ca_attr = r_data[['ca_attr']]) {
	for(it in 1:20) {
		cmd <- "ca.attributes <- list(c()"
		for(l in ca_attr) {
			if(l != "") cmd <- paste(cmd, ",", l)
		}
		cmd <- paste(cmd, ")")
		eval(parse(text = cmd))

		ret <- conjointFFD(ca.attributes[-1], it)
		if(!is.null(ret)) break

		# reordering the attributes has an effect on
		# the number of profiles generated
		ca_attr <- sample(ca_attr)
	}

	pro.cor <- cor(data.matrix(ret$frac))

	Profile <- 1:dim(ret$frac)[1]
	FFdesign <- cbind(Profile, ret$frac)

	Profile <- 1:dim(ret$full)[1]
	CFdesign <- cbind(Profile, ret$full)

	attr <- ca_attr
	frac <- FFdesign
	full <- CFdesign

	# return(list(attr = ca_attr, pro.cor = pro.cor, frac = FFdesign, full = CFdesign))
	environment() %>% as.list %>% set_class(c("conjoint_profiles",class(.)))
}

# summary_conjointProfiles <- function(result = .conjoint_profiles()) {
summary.conjoint_profiles <- function(result) {

	cat("Generate conjoint profiles\n")

	cat("\nAttributes and levels:\n")
	cat(paste0(result$attr, collapse="\n"),"\n")

	cat(paste("\nThe number of profiles selected is equal to", nrow(result$frac)))

	cnames <- strsplit(result$attr, " ")
	cn <- c("Profile")
	for(i in cnames) cn <- c(cn, i[1])

	cat("\n\nFractional factorial design correlations:\n")
	print(result$pro.cor, row.names = FALSE)

	cat("\nFractional factorial design:\n")
	print(result$frac[,cn] %>% arrange_(.dots = cn[-1]) %>% mutate(Profile = 1:n()), row.names = FALSE)

	cat("\nFull factorial design:\n")
	print(result$full[,cn], row.names = FALSE)
}

conjointFFD <- function(dat, trial = 50, rseed = 172110) {

	experiment <- expand.grid(dat)

	###############################################
	# eliminate combinations from experiment
	# by removing then from the 'experiment'
	# http://stackoverflow.com/questions/18459311/creating-a-fractional-factorial-design-in-r-without-prohibited-pairs?rq=1
	###############################################

	nr_levels <- sapply(dat, length) %>% sum
	min_profiles <- nr_levels - length(dat) + 1
	max_profiles <- nrow(experiment)

	for (i in min_profiles:max_profiles) {
		set.seed(rseed) 		# need to be in the look
		design <- AlgDesign::optFederov(data = experiment, nTrials=i, maxIteration=1000)
		cor_mat <- cor(data.matrix(design$design))
		# cat('\nEvaluating the',i,'profile design\n\n')
		# print(as.dist(cor_mat), digits = 1)
		# cat('\nD-efficiency:',design$Dea,'\n')
		if(det(cor_mat)==1) break
		# if(design$Dea == 1) break
	}

	nr_profiles <- design$design %>% nrow
	# cat(paste("\nThe number of profiles selected is equal to",nr_profiles,"\n\n"))
	if(nr_profiles> 24) {
		if(trial < 20) {
			return()
		} else {
			cat(paste("The number of profiles required to generate an orthogonal design
				is greater than the recommended maximum of 24. Consider reducing the number
				of attributes and/or levels.\n"))
		}
	}

	list(frac = design$design, full = experiment)
}

observe({
  if(!is.null(input$uploadAttr)) {
    isolate({
      r_data[['ca_attr']] <- gsub("\"","\'",readLines(input$uploadAttr$datapath))
    })
  }
})

output$downloadProfiles <- downloadHandler(
	filename = function() { 'conjoint_profiles.csv' },
  content = function(file) {
		.conjoint_profiles() %>%
		{ if(class(.)[1] == "character") . else .$frac } %>%
		write.csv(file, row.names = FALSE)
	}
)

# library(DoE.base)
# # prof <- read.csv("~/Desktop/conjoint_profiles.csv") %>%
# prof <- structure(list(sight = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L,
# 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("Not Staggered",
# "Staggered"), class = "factor"), food = structure(c(1L, 1L, 1L,
# 2L, 2L, 2L, 3L, 3L, 3L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L), .Label = c("Gourmet food",
# "Hot dogs and popcorn", "No food"), class = "factor"), price = structure(c(1L,
# 1L, 2L, 2L, 2L, 3L, 1L, 3L, 3L, 2L, 3L, 3L, 1L, 1L, 3L, 1L, 2L,
# 2L), .Label = c("$10", "$13", "$16"), class = "factor"), comfort = structure(c(1L,
# 3L, 3L, 1L, 2L, 1L, 2L, 2L, 3L, 2L, 1L, 2L, 2L, 3L, 3L, 1L, 1L,
# 3L), .Label = c("Average cup", "Average no cup", "Large cup"), class = "factor"),
#     audio.visual = structure(c(1L, 3L, 2L, 2L, 3L, 1L, 3L, 1L,
#     2L, 1L, 3L, 2L, 2L, 1L, 3L, 2L, 3L, 1L), .Label = c("Large digital",
#     "Large plain", "Small plain"), class = "factor")), class = "data.frame", row.names = c(NA,
# -18L), .Names = c("sight", "food", "price", "comfort", "audio.visual")) %>%
#     select(sight, food, price, comfort, audio.visual) %>%
#     arrange(sight, food, price, comfort, audio.visual)

# lev <- list(price = c('$10','$13','$16'),
#             sight = c('Staggered','Not Staggered'),
#             comfort = c('Average no cup','Average cup','Large cup'),
#             audio.visual = c('Small plain','Large plain','Large digital'),
#             food = c('No food','Hot dogs and popcorn','Gourmet food'))

# to_pad <- sapply(lev, length)
# max(to_pad) == min(to_pad)
# for(i in 1:length(lev)) {
# 	lev[[i]] <- c(lev[[i]], rep("",max(to_pad)-length(lev[[i]])))
# }

# write.csv(lev, "~/Desktop/test.csv", row.names = FALSE)
# read.csv(test, colClasses = "character")

# oa.design(seed = 1234, factor.names = sample(lev), columns="min34") %>%
#   select(sight, food, price, comfort, audio.visual) %>%
#   arrange(sight, food, price, comfort, audio.visual)

# cor_mat <- cor(data.matrix(res))
# cor_mat
# cat(paste(nrow(res)," ", det(cor_mat)), "\n")

# library(DoE.base)
# library(dplyr)
# lev <- list(price = c('$10','$13','$16'),
#             sight = c('Staggered','Not Staggered'),
#             comfort = c('Average no cup','Average cup','Large cup'),
#             audio.visual = c('Small plain','Large plain','Large digital'),
#             food = c('No food','Hot dogs and popcorn','Gourmet food'))

# oa.design(seed = 1234, factor.names = sample(lev), columns="min34") %>%
#   select(sight, food, price, comfort, audio.visual) %>%
#   arrange(sight, food, price, comfort, audio.visual)

# cor_mat <- cor(data.matrix(res))
# cor_mat
# cat(paste(nrow(res)," ", det(cor_mat)), "\n")

