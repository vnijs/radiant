output$logwork <- renderPrint({
	# if(input$datatabs != 'Log') return()

	# Idea: When a user presses a log-button the output on screen is saved to an rda file
	# ala the sesson data (.Rdata), i.e., take a snap-shot of the input list
	# Snap shots are shown in log but user can deselect snap-shots as desired.
	# Take another look at Jeff's teaching log. this could be a great starting point.
	# Ask Jeff about how to attribute code (and also Yihie) if you use some of their code
	# https://github.com/jeffreyhorner/TeachingLab

	cat("Reproducible research using Shiny (in development)\n")
})