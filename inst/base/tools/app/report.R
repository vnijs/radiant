################################################################
# Create dynamic reports using Radiant and the shinyAce editor
################################################################
rmd_switch <- c("Switch tab", "Don't switch tab")
rmd_manual <- c("Manual paste", "Auto paste")

rmd_example <- "## Sample report

This is an example of the type of report you can write in Radiant.

* You can create
* bullet lists

1. And numbered
2. lists

### Math

You can even include math if you want:

$$y_t = \\alpha + \\beta x_t + \\epsilon_t.$$

To show the output press the `Knit report` button.

### Documenting analysis results in Radiant

The report feature in Radiant should be used in conjunction with the <i title='Report results' class='fa fa-edit'></i> icons shown at the bottom of the side bar on (almost) all pages. When that icon is clicked the command used to create the ouput is copied into the editor in the R > Report. By default Radiant will paste the code generated for the analysis you just completed at the bottom of the report. However, you can turn off that feature by clicking the `Manual paste (off)` button. The text in the button should now read `Manual paste (on)`. Click the button again to turn manual paste off again. With manual paste on the code is put in the clipboard when you click a report icon and you can paste it where you want in the R > Report editor window.

By clicking the `Knit report` button, the output from the analysis will be recreated. You can add text, bullets, headers, etc. around the code blocks to describe and explain the results using <a href='http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html' target='_blank'>markdown</a>.

Below is some code created in Radiant that will generate regression outputfor the _diamonds_ data. These are histograms and a scatterplot / heatmap of the price of diamonds versus carats. The colors in the plot reflect the clarity of the diamond.

```{r fig.width=7, fig.height=4}
result <- regression(dataset = 'diamonds', rvar = 'price', evar = 'carat')
summary(result)
plot(result, plots = 'hist')
```

```{r fig.width=7, fig.height=7}
visualize(dataset = 'diamonds', xvar = 'carat', yvar = 'price',
          type = 'scatter', color = 'clarity')
```

> **Put your own code here or delete this sample report and create your own**
"

# observeEvent(input$manual_paste, {
#   r_data$manual %<>% {. == FALSE}
# })

# output$ui_manual <- renderUI({
#   ## initialize manual cmd paste to false
#   if (is.null(r_data$manual)) r_data$manual <- FALSE
#   actionButton("manual_paste",
#     if (r_data$manual) "Manual paste (on)" else "Manual paste (off)")
# })

output$ui_rmd_manual <- renderUI({
  ## initialize manual cmd paste to false
  # if (is.null(r_data$manual)) r_data$manual <- FALSE
  selectInput(inputId = "rmd_manual", label = NULL,
    choices = rmd_manual,
    selected = state_init("rmd_manual", "Auto paste"),
    multiple = FALSE, selectize = FALSE,
    width = "120px")
})

output$ui_rmd_switch <- renderUI({
  selectInput(inputId = "rmd_switch", label = NULL,
    choices = rmd_switch,
    selected = state_init("rmd_switch", "Switch tab"),
    multiple = FALSE, selectize = FALSE,
    width = "140px")
})

esc_slash <- function(x) gsub("([^\\])\\\\([^\\\\$])","\\1\\\\\\\\\\2",x)

output$report <- renderUI({
  init <- isolate(if (is_empty(input$rmd_report)) rmd_example else esc_slash(input$rmd_report))
  tagList(
    with(tags,
      table(
            td(help_modal('Report','report_help',
                       inclMD(file.path(r_path,"base/tools/help/report.md")))),
            td(HTML("&nbsp;&nbsp;")),
            td(actionButton("evalRmd", "Knit report"), style= "padding-top:5px;"),
            # td(uiOutput("ui_manual")),
            td(uiOutput("ui_rmd_manual")),
            td(uiOutput("ui_rmd_switch")),
            td(downloadButton("saveHTML", "Save HTML"), style= "padding-top:5px;"),
            td(downloadButton("saveRmd", "Save Rmd"), style= "padding-top:5px;"),
            td(HTML("<div class='form-group shiny-input-container'>
                <input id='load_rmd' name='load_rmd' type='file' accept='.rmd,.Rmd,.md'/>
              </div>"))
      )
    ),

    shinyAce::aceEditor("rmd_report", mode = "markdown",
              vimKeyBinding = ifelse (is.null(r_data$vim_keys), FALSE, r_data$vim_keys),
              wordWrap = TRUE,
              height = "auto",
              selectionId = "rmd_selection",
              value = state_init("rmd_report", init) %>% esc_slash,
              hotkeys = list(runKeyRmd = list(win = "CTRL-ENTER", mac = "CMD-ENTER"))),
    htmlOutput("rmd_knitted")
  )
})

valsRmd <- reactiveValues(knit = 0)

observe({
  input$runKeyRmd
  if (!is.null(input$evalRmd)) isolate(valsRmd$knit %<>% add(1))
})

scrub <-
  . %>%
  gsub("&lt;!--/html_preserve--&gt;","",.) %>%
  gsub("&lt;!--html_preserve--&gt;","",.) %>%
  gsub("&lt;!&ndash;html_preserve&ndash;&gt;","",.) %>%
  gsub("&lt;!&ndash;/html_preserve&ndash;&gt;","",.)  ## knitr adds this

## Knit to save html
knitIt <- function(text) {
  knitr::knit2html(text = text, quiet = TRUE, envir = r_knitr,
                   options=c("mathjax", "base64_images"),
                   stylesheet = file.path(r_path,"base/www/bootstrap.min.css")) %>%
  scrub %>% HTML
}

## Knit for report in Radiant
knitIt2 <- function(text) {
  paste(knitr::knit2html(text = text, fragment.only = TRUE, quiet = TRUE,
        envir = r_knitr), stylesheet = "",
        "<script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>",
        "<script>if (window.MathJax) MathJax.Hub.Typeset();</script>", sep = '\n') %>% scrub %>% HTML
}

output$rmd_knitted <- renderUI({
  req(valsRmd$knit != 1)
  # req(input$evalRmd || (input$evalRmd >= 0 && !is.null(input$runKeyRmd$randNum)))
  isolate({
    if (!r_local) {
      return(HTML("<h2>Rmd file is not evaluated when running Radiant on a server</h2>"))
    }
    if (input$rmd_report != "") {
      withProgress(message = "Knitting report", value = 0, {
        if (is_empty(input$rmd_selection))
          knitIt2(input$rmd_report)
        else
          knitIt2(input$rmd_selection)
      })
    }
  })
})

output$saveHTML <- downloadHandler(
  filename = function() {"report.html"},
  content = function(file) {
    if (r_local) {
      isolate({
        withProgress(message = "Knitting report", value = 0, {
          ifelse (is_empty(input$rmd_selection), input$rmd_report,
                  input$rmd_selection) %>%
            knitIt %>% cat(file=file,sep="\n")
        })
      })
    }
  }
)

output$saveRmd <- downloadHandler(
  filename = function() {"report.zip"},
  content = function(fname) {
    isolate({

      cdir <- getwd()
      setwd(tempdir())
      fbase <- "report"
      fnames <- c("report.Rmd", "r_data.rda")

      paste0("```{r echo = FALSE}\nknitr::opts_chunk$set(comment=NA, echo = FALSE, cache=FALSE, message=FALSE, warning=FALSE)\nsuppressWarnings(suppressMessages(library(radiant)))\nload(\"", fnames[2], "\")\n```\n\n") %>%
        paste0(., input$rmd_report) %>% gsub("\\\\\\\\","\\\\",.) %>% cat(., file = fnames[1],sep="\n")

      r_data <- reactiveValuesToList(r_data)
      save(r_data, file = fnames[2])

      zip(fname, fnames[1:2])
      setwd(cdir)
    })
  },
  contentType = "application/zip"
)

observeEvent(input$load_rmd, {
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
      inp <- "result"
      if ("result" %in% names(inp_out[[i]])) {
        inp <- inp_out[[i]]["result"]
        inp_out[[i]]["result"] <- NULL
      }
      if (inp_out[i] != "" && length(inp_out[[i]]) > 0) {
        cmd <- deparse(inp_out[[i]], control = c("keepNA"), width.cutoff = 500L) %>%
                 sub("list\\(", paste0(outputs[i], "\\(", inp, ", "), .) %>%
                 paste0(cmd, "\n", .)
      } else {
        cmd <- paste0(cmd, "\n", outputs[i], "(", inp, ")")
      }
    }
  }

  if (xcmd != "") cmd <- paste0(cmd, "\n", xcmd)

  if (figs)
    cmd <- paste0("\n```{r fig.width=",fig.width,", fig.height=",fig.height,", dpi = 72}\n",cmd,"\n```\n")
  else
    cmd <- paste0("\n```{r}\n",cmd,"\n```\n")

  update_report_fun(cmd)
}

observeEvent(input$rmd_report, {
  if (input$rmd_report != rmd_example)
    r_state$rmd_report <<- esc_slash(input$rmd_report)
})

update_report_fun <- function(cmd) {
  # req(input$rmd_manual)
  # req(input$rmd_switch)
  isolate({
    # if (isolate(r_data$manual)) {
    # if (input$rmd_manual == "Manual paste") {
    # if (use_input_nonvar("rmd_manual", rmd_manual, "Auto paste") == "Manual paste") {
    if (state_init("rmd_manual", "Auto paste") == "Manual paste") {
      os_type <- Sys.info()["sysname"]
      if (os_type == 'Windows') {
        cat(cmd, file = "clipboard")
      } else if (os_type == "Darwin") {
        out <- pipe("pbcopy")
        cat(cmd, file = out)
        close(out)
      } else if (os_type == "Linux") {
        cat("Clipboard not supported on linux")
      }

      withProgress(message = 'Putting command in clipboard', value = 0,
        cat("")
        # shinyAce::updateAceEditor(session, "rmd_report",
        #                           value = esc_slash(r_state$rmd_report))
      )

      ## nothing is added to report
      # updateTabsetPanel(session, "nav_radiant", selected = "Report")
    } else {
      if (is_empty(r_state$rmd_report)) {
        r_state$rmd_report <<- paste0("## Your report title\n", cmd)
      } else {
        r_state$rmd_report <<- paste0(esc_slash(r_state$rmd_report),"\n",cmd)
      }

      withProgress(message = 'Updating report', value = 0,
        shinyAce::updateAceEditor(session, "rmd_report",
                                  value = esc_slash(r_state$rmd_report))
      )
    }

    # if (use_input_nonvar("rmd_switch", rmd_switch, "Switch tab") == "Switch tab")
    if (state_init("rmd_switch", "Switch tab") == "Switch tab")
      updateTabsetPanel(session, "nav_radiant", selected = "Report")
  })
}
