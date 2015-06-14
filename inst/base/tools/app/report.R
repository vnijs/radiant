################################################################
# Create dynamic reports using Radiant and the shinyAce editor
################################################################
rmd_example <- "## Sample report

This is an example of the type of report you can write in Radiant.

* You can create
* bullet lists

1. And numbered
2. lists

### Math

You can even include math if you want:

$$y_t = \\alpha + \\beta x_t + \\epsilon_t.$$

To show the output press the `Update` button.

### Documenting analysis results in Radiant

The report feature in Radiant should be used in conjunction with the <i title='Report results' class='glyphicon glyphicon-book'></i> icons shown on the bottom of the (left) side bar on all analytisis pages. When that icon is clicked the command used to create the ouput is copied into the editor in the R > Report. By default Radiant will paste the code generated for the analysis you just completed at the bottom of the report. However, you can turn off that feature by clicking the `Manual paste (off)` button. The text in the button should now read `Manual paste (on)`. Click the button again to turn manual paste off again. With manual paste on the code is put in the clipboard when you click a book icon and you can paste it where you want in the R > Report editor window.

By clicking the update button, the output from the analysis will be recreated. You can add text, bullets, headers, etc. around the code blocks to describe and explain the results using <a href='http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html' target='_blank'>markdown</a>.

Below is some code created in Radiant that will generate regression outputfor the _diamonds_ data. These are histograms and a scatterplot / heatmap of the price of diamonds versus carats. The colors in the plot reflect the clarity of the diamond.

```{r fig.width=7, fig.height=4}
result <- regression(dataset = 'diamonds', dep_var = 'price',
                     indep_var = 'carat')
summary(result)
plot(result, plots = 'hist')
```

```{r fig.width=7, fig.height=7}
visualize(dataset = 'diamonds', xvar = 'carat', yvar = 'price',
          type = 'scatter', color = 'clarity')
```
"

observeEvent(input$manual_paste, {
  isolate(r_data$manual %<>% {. == FALSE})
})

output$ui_manual <- renderUI({
  ## initialize manual cmd paste to false
  if(is.null(r_data$manual)) r_data$manual <- FALSE
  actionButton("manual_paste",
    if(r_data$manual) "Manual paste (on)" else "Manual paste (off)")
})

output$report <- renderUI({
  tagList(
    with(tags,
      table(
            td(help_modal('Report','report_help',
                       inclMD(file.path(r_path,"base/tools/help/report.md")))),
            td(HTML("&nbsp;&nbsp;")),
            td(actionButton("evalRmd", "Update")),
            td(uiOutput("ui_manual")),
            td(downloadButton("saveHTML", "Save HTML")),
            td(downloadButton("saveRmd", "Save Rmd")),
            td(HTML("<div class='form-group shiny-input-container'>
                <input id='load_rmd' name='load_rmd' type='file' accept='.rmd,.Rmd,.md'/>
              </div>"))
      )
    ),

    shinyAce::aceEditor("rmd_report", mode = "markdown",
              wordWrap = TRUE,
              height = "auto",
              selectionId = "rmd_selection",
              value = state_init("rmd_report",rmd_example),
              hotkeys = list(runKeyRmd = list(win = "CTRL-ENTER", mac = "CMD-ENTER"))),
    htmlOutput("rmd_knitted")
  )
})

valsRmd <- reactiveValues(knit = 0)

observe({
  input$runKeyRmd
  if (!is.null(input$evalRmd)) isolate(valsRmd$knit %<>% add(1))
})

## Knit to save html
knitIt <- function(text) {
  knitr::knit2html(text = text, quiet = TRUE, , envir = r_env,
                   options=c("mathjax", "base64_images"),
                   stylesheet = file.path(r_path,"base/www/rmarkdown.css")) %>% HTML
}

## Knit for report in Radiant
knitIt2 <- function(text) {
  paste(knitr::knit2html(text = text, fragment.only = TRUE, quiet = TRUE, envir = r_env),
        "<script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>",
        "<script>MathJax.Hub.Typeset();</script>", sep = '\n') %>% HTML
}

output$rmd_knitted <- renderUI({
  if (valsRmd$knit == 1) return()

  isolate({
    if (!r_local) {
      return(HTML("<h2>Rmd file is not evaluated when running Radiant on a server</h2>"))
    }
    if (input$rmd_report != "") {
      withProgress(message = 'Knitting report', value = 0, {
        ifelse(is.null(input$rmd_selection) || input$rmd_selection == "",
               return(knitIt2(input$rmd_report)),
               return(knitIt2(input$rmd_selection)))
      })
    }
  })
})

output$saveHTML <- downloadHandler(
  filename = function() {"report.html"},
  content = function(file) {
    if (r_local) {
      isolate({
        ifelse(is.null(input$rmd_selection) || input$rmd_selection == "",
               text <- input$rmd_report, text <- input$rmd_selection)
        knitIt(text) %>% cat(.,file=file,sep="\n")
      })
    }
  }
)

output$saveRmd <- downloadHandler(
  filename = function() {"report.Rmd"},
  content = function(file) {
    isolate({
      "```{r echo = FALSE}\n# knitr::opts_chunk$set(comment=NA, cache=FALSE, message=FALSE, warning=FALSE)\n# suppressMessages(library(radiant))\n# uncomment the lines above to 'knit' the Rmd file in Rstudio\n# you will also need to load the data using load()\n```\n\n" %>%
        paste0(.,input$rmd_report) %>% cat(.,file=file,sep="\n")
    })
  }
)

observe({
  ## loading rmd report from disk
  inFile <- input$load_rmd
  if (!is.null(inFile) && !is.na(inFile)) {
    isolate({
      rmdfile <- paste0(readLines(inFile$datapath), collapse = "\n")
      shinyAce::updateAceEditor(session, "rmd_report", value = rmdfile)
    })
  }
})

## updating the report when called
update_report <- function(inp_main = "", fun_name = "", inp_out = list("",""),
                          pre_cmd = "result <- ", post_cmd = "", xcmd = "",
                          outputs = c("summary", "plot"),
                          figs = TRUE, fig.width = 7, fig.height = 7) {

  cmd <- ""
  if (inp_main[1] != "") {
    cmd <- deparse(inp_main, control = c("keepNA"), width.cutoff = 500L) %>%
             paste(collapse="") %>%
             sub("list", fun_name, .) %>%
             paste0(pre_cmd, .) %>%
             paste0(., post_cmd)
  }

  lout <- length(outputs)
  if (lout > 0) {
    for (i in 1:lout) {
      if (inp_out[i] != "" && length(inp_out[[i]]) > 0) {
        cmd <- deparse(inp_out[[i]], control = c("keepNA"), width.cutoff = 500L) %>%
                 sub("list\\(", paste0(outputs[i], "\\(result, "), .) %>%
                 paste0(cmd, "\n", .)
      } else {
        cmd <- paste0(cmd, "\n", outputs[i], "(result)")
      }
    }
  }

  if (xcmd != "") cmd <- paste0(cmd, "\n", xcmd)

  if (figs)
    cmd <- paste0("\n```{r fig.width=",fig.width,", fig.height=",fig.height,"}\n",cmd,"\n```\n")
  else
    cmd <- paste0("\n```{r}\n",cmd,"\n```\n")

  update_report_fun(cmd)
}

update_report_fun <- function(cmd) {

  if (isolate(r_data$manual)) {
    os_type <- Sys.info()["sysname"]
    if (os_type == 'Windows') {
      cat(cmd, file = "clipboard")
    } else if (os_type == "Darwin") {
      cat(cmd, file = pipe("pbcopy"))
    } else if (os_type == "Linux") {
      cat("Clipboard not supported on linux")
    }
    ## nothing is added to report
    cmd <- ""
  }

  if (cmd != "") {
    if (is.null(input$rmd_report)) {
      if (is.null(r_state$rmd_report)) {
        r_state$rmd_report <<- cmd
      } else {
        r_state$rmd_report <<- paste0(r_state$rmd_report,"\n",cmd)
      }
    } else {
      shinyAce::updateAceEditor(session, "rmd_report",
                      value = paste0(input$rmd_report,"\n",cmd))
    }
  }

  ## move to the report panel so see the commands created
  updateTabsetPanel(session, "nav_radiant", selected = "Report")
}
