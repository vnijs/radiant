################################################################
# Create dynamic reports using Radiant and the shinyAce editor
################################################################
rmd_example <- "## Sample report in Markdown

This is an example report in markdown. You can embed R code to be executed.

```{r}
rnorm(5)
x <- 14*3
```

### Inline code
Inline R code, e.g., the value of x is `r x`.

### Math
LaTeX math:  $f(\\\\alpha, \\\\beta) \\\\propto x^{\\\\alpha-1}(1-x)^{\\\\beta-1}$.

### Figures
```{r}
hist(rnorm(100))
```
"

# To include html or markdown created by R use results='asis'
# ```{r results='asis'}
# cat('_Markdown_,', 'oh yeah, **Markdown**')
# ```

opts_chunk$set(echo=FALSE, comment=NA, cache=TRUE, message=FALSE, warning=FALSE,
# opts_chunk$set(echo=FALSE, comment=NA, message=FALSE, warning=FALSE,
  fig.path = "www/rmd/figure/", cache.path = "www/rmd/cache/")
opts_knit$set(progress = FALSE)

output$report <- renderUI({
  div(class="row-fluid", div(class="span6",
    aceEditor("rmd_report", mode="markdown", value=state_init("rmd_report",rmd_example)),
      # vimKeyBinding=vimKeyBinding),
    actionButton("evalRmd", "Update"),
    downloadButton('saveHTML', 'Save HTML'), 
    downloadButton('saveRmd', 'Save Rmd'), tags$br(), tags$br(),
    fileInput('loadRmd', 'Load Rmd', multiple=TRUE)),
    # HTML("<a 'class='link action-button shiny-bound-input' id='gotoData'>Test</a>"),
    div(class="span6", 
      htmlOutput("rmd_knitDoc"),
      div(class = "busy",
        p("Calculation in progress ..."),
        img(src="ajaxloaderq.gif")
      )
    )
  )
})

# observe({
#   if(is.null(input$renameButton) || input$renameButton == 0) return()
#   updateTabsetPanel(session, "nav_radiant", selected = "Report") 
# })

output$rmd_knitDoc <- renderUI({
  if(is.null(input$evalRmd) || input$evalRmd == 0) return() 
    isolate({
      if(running_local && input$rmd_report != "")
        return(HTML(paste(knit2html(text = input$rmd_report, fragment.only = TRUE, quiet = TRUE), '<script>', 'MathJax.Hub.Typeset();', '</script>', sep = '\n')))
      if(!running_local)
        return(HTML("<h2>Rmd file is not evaluated when running Radiant on a server</h2>"))
    })
})  

output$saveHTML <- downloadHandler(
  filename = function() {"report.html"},
  content = function(file) {
    if(running_local) {
      isolate({
        html <- knit2html(text = input$rmd_report, quiet = TRUE, options=c("mathjax", "base64_images"))
        cat(html,file=file,sep="\n")
      })
    }
  }
)

output$saveRmd <- downloadHandler(
  filename = function() {"report.Rmd"},
  content = function(file) {
    isolate({
      cat(input$rmd_report,file=file,sep="\n")
    })
  }
)

observe({

  # Useful to jump to reporting tab on refresh when testing
  # updateTabsetPanel(session, "nav_radiant", selected = "Report") 

  # loading r-code from disk
  inFile <- input$loadRmd
  if(!is.null(inFile) && !is.na(inFile)) {
    isolate({
      rmdfile <- paste0(readLines(inFile$datapath), collapse = "\n")
      updateAceEditor(session, "rmd_report", value = rmdfile)
    })
  }
})

# updating the report when called
updateReport <- function(inp, fun_name, fig.width = 7, fig.height = 7, xcmd = "",
  sum_name = paste0("summary_",fun_name), plots_name = paste0("plots_",fun_name)) {

  cmd <- paste0("result <- ", sub('list',fun_name, deparse(inp, control = c("keepNA"), width.cutoff = 500L)),
    collapse="\n")
  cmd <- paste0(cmd, "\n", sum_name,"(result)\n", plots_name,"(result)")
  if(xcmd != "") cmd <- paste0(cmd, "\n", xcmd)
  cmd <- paste0("\n```{r fig.width=",fig.width,", fig.height=",fig.height,"}\n",cmd,"\n```\n")
  updateReportFun(cmd)
}

updateReportViz <- function(inp, fun_name, fig.width = 7, fig.height = 7, xcmd = "") {

  cmd <- sub('list',fun_name, deparse(inp, control = c("keepNA"), width.cutoff = 500L))
  if(xcmd != "") cmd <- paste0(cmd, "\n", xcmd)
  cmd <- paste0("\n```{r fig.width=",fig.width,", fig.height=",fig.height,"}\n",cmd,"\n```\n")
  updateReportFun(cmd)
}

updateReportMerge <- function(inp, fun_name) {

  cmd <- sub('list',fun_name, deparse(inp, control = c("keepNA"), width.cutoff = 500L))
  cmd <- paste0("\n```{r }\n",cmd,"\n```\n")
  updateReportFun(cmd)
}

updateReportFun <- function(cmd) {
 if(is.null(input$rmd_report)) {
    if(is.null(state_list$rmd_report)) {
      state_list$rmd_report <<- cmd
    } else {
      state_list$rmd_report <<- paste0(state_list$rmd_report,"\n",cmd)
    }
  } else {
    updateAceEditor(session, "rmd_report", value = paste0(input$rmd_report,"\n",cmd))
  }

  # move to the report panel
  updateTabsetPanel(session, "nav_radiant", selected = "Report") 
}


################################################################
# Run R-code within Radiant using the shinyAce editor
################################################################
r_example <- "# to get the currently active data
dat <- getdata()  

# show the first observations
head(dat)

# to access a specific dataset by name
dat <- values[['diamonds']] 

# add a variable to the data used by Radiant
values[['diamonds']]$log.price <- log(dat$price)
dat <- values[['diamonds']] 

# show the first observations
head(dat)

# run a regression
reg <- lm(price ~ carat + clarity, data = dat)
summary(reg)
"

output$rcode <- renderUI({
  div(class="row-fluid", div(class="span6",
      aceEditor("r_code", mode="r", value=state_init("r_code",r_example)),
        # vimKeyBinding=vimKeyBinding),
      actionButton("rEval", "Run"),
      downloadButton('saveCode', 'Save R-code'), tags$br(), tags$br(),
      fileInput('sourceCode', 'Source R-code', multiple=TRUE),
      fileInput('loadCode', 'Load R-code', multiple=FALSE)
    ),
    div(class="span6", htmlOutput("rCodeEval"))
  )
})

output$rCodeEval <- renderPrint({
  if(is.null(input$rEval) || input$rEval == 0) return(invisible()) 
  if(running_local) {
    return(isolate(HTML(knit2html(text = paste0("```{r cache=FALSE}\n",input$r_code,"\n```"),
      fragment.only = TRUE, quiet = TRUE))))
  } else {
    return(HTML("<h2>Code is not evaluated when running Radiant on a server</h2>"))
  }
})  

output$saveCode <- downloadHandler(
  filename = function() {"rcode.R"},
  content = function(file) {
    isolate({
      cat(input$r_code,file=file,sep="\n")
    })
  }
)

observe({
  # loading r-code from disk
  inFile <- input$loadCode
  if(!is.null(inFile) && !is.na(inFile)) {
    isolate({
      rfile <- paste0(readLines(inFile$datapath), collapse = "\n")
      updateAceEditor(session, "r_code", value = rfile)
    })
  }
})

observe({
  # source r-code from disk
  inFile <- input$sourceCode
  if(!is.null(inFile) && !is.na(inFile)) {
    isolate({
      source(inFile$datapath)
    })
  }
})
