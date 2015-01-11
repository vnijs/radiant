# options(rstudio.markdownToHTML =
#   function(inputFile, outputFile) {     
#     require(markdown)
#     markdownToHTML(inputFile, outputFile, options = c(""), stylesheet='empty.css')  
#   }
# )

# require(knitr)
# helpfiles <- list.files(".", pattern = "*.Rmd")
# for(hf in helpfiles) {
# 	knit2html(hf, options = "", stylesheet = "empty.css")
# }

#     require(markdown)
#     markdownToHTML(inputFile, outputFile, options = c(""), stylesheet='empty.css')  

helpfiles <- list.files(".", pattern = "*.md")
for(hf in helpfiles) {
	knit2html(hf, options = "", stylesheet = "empty.css")
	# markdowntohtml(inputfile, outputfile, options = c(""), stylesheet='empty.css')  
}

