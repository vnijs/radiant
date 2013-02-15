output$logwork <- renderPrint({
	# if(input$datatabs != 'Log') return()
	# idea: When a user presses a log-button the output on screen is saved to an rda file
	# ala the sesson data (.Radata). It would be like taking a snap-shot of the app-input
	# and then call the relevant parts from an Rmd file that gets-sourced. By default all snap
	# shots are shown in log but user can deseleted snap-shots as desired.
	# take another look at Jeff's teaching log. this could be a great starting point
	# ask Jeff about how to attribute code (and also Yihie) if you use some of their code
	# https://github.com/jeffreyhorner/TeachingLab

	cat("Reproducible research using Shiny (in development)\n")
})