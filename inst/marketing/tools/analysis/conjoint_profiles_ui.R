###############################
# Conjoint profiles
###############################
output$ui_conjoint_profiles <- renderUI({
  list(
  	wellPanel(
			fileInput('cap_upload', 'Upload attributes:', multiple=FALSE),
      # conditionalPanel(condition = "input.cap_upload != null",
	  		downloadButton('cap_download_profiles', 'Save profiles')
	  	# )
		),
  	help_and_report(modal_title = "Conjoint profiles",
  	                fun_name = "conjoint_profiles",
  	                help_file = inclMD("tools/help/conjoint_profiles.md"))
	)
})

# output is called from the main radiant ui.R
output$conjoint_profiles <- renderUI({

		register_print_output("summary_conjoint_profiles", ".summary_conjoint_profiles")

		cap_output_panels <- tagList(
	     tabPanel("Summary", verbatimTextOutput("summary_conjoint_profiles"))
	  )

		stat_tab_panel(menu = "Conjoint",
		               tool = "Create profiles",
		               tool_ui = "ui_conjoint_profiles",
		               output_panels = cap_output_panels)
})

.conjoint_profiles <- reactive({
	conjoint_profiles("cap_attr")
})

.summary_conjoint_profiles <- reactive({
	ret_text <- "Please load a file with attribute information. For an example see\nhttps://github.com/vnijs/radiant/blob/master/inst/examples/profiles-movie.txt"
	if (is.null(input$cap_upload)) return(ret_text)
  if (is.null(r_data[["cap_attr"]])) return(ret_text)

	summary(.conjoint_profiles())
})

observe({
  if (not_pressed(input$conjoint_profiles_report)) return()
  isolate({
    xcmd <- "# write.csv(result$frac, file = '~/conjoint_profiles.csv', row.names = FALSE)"
    update_report(inp_main = list(dataset = "cap_attr"),
                  fun_name = "conjoint_profiles",
                  inp_out = list("",""), outputs = "summary",
                  figs = FALSE, xcmd = xcmd)
  })
})

observe({
  if (!is.null(input$cap_upload)) {
    isolate({
      r_data[["cap_attr"]] <- gsub("\"","\'",readLines(input$cap_upload$datapath))
    })
  }
})

output$cap_download_profiles <- downloadHandler(
	filename = function() { 'conjoint_profiles.csv' },
  content = function(file) {
		.conjoint_profiles() %>%
		{ if (class(.)[1] == "character") . else .$frac } %>%
		write.csv(file, row.names = FALSE)
	}
)
