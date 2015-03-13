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

You can even include math if you want: $f(\\alpha, \\beta) \\propto x^{\\alpha-1}(1-x)^{\\beta-1}$.

To show the output press the `Update` button.

### Documenting analysis results in Radiant

When you click the book icon on a page the browser will bring you
to this report page. By default Radiant will paste the code generated
for the analysis you just completed at the bottom of the report.
However, you can turn off that feature by clicking the `Manual paste`
checkbox. The code will then be put in the clipboard when you click a
book icon and you can paste it where you want in the editor window.

Below is some code created in Radiant will generate regression output
for the `diamonds` data. There are plots of histograms and a scatterplot
/ heatmap of the price of diamonds versus carats. The colors in the plot
reflect the clarity of the diamond.

```{r fig.width=7, fig.height=3.5}
result <- regression('diamonds', 'price', 'carat', NULL, NULL, 'none',
                     'dataframe', '', 'none', FALSE, FALSE, FALSE, 0.95,
                     FALSE, FALSE, FALSE, 'histlist', FALSE, FALSE)
summary_regression(result)
plots_regression(result)
```

```{r fig.width=7, fig.height=7}
visualize('diamonds', 'price', 'carat', '', 'single', '.', '.', 'clarity', FALSE, FALSE, FALSE)
```
"

knitr::opts_chunk$set(echo=FALSE, comment=NA, cache=FALSE, message=FALSE, warning=FALSE,
               fig.path = "~/radiant_figures/")
               # fig.path = "~/radiant_temp/rmd/figure/")
knitr::opts_knit$set(progress = TRUE)

output$report <- renderUI({

  list(
    actionButton("evalRmd", "Update"),
    downloadButton("saveHTML", "Save HTML"),
    downloadButton("saveRmd", "Save Rmd"),
    checkboxInput("manualPaste", "Manual paste",
                  value = state_init("manualPaste", FALSE)),
    fileInput("loadRmd", "", multiple=TRUE),

    div(class="row",
      div(class="col-xs-6",
        aceEditor("rmd_report", mode="markdown", wordWrap = TRUE,
                  height = "600px",
                  selectionId = "rmd_selection", value=state_init("rmd_report",rmd_example),
                  hotkeys=list(runKeyRmd=list(win="Ctrl-R|Ctrl-Shift-Enter", mac="CMD-ENTER|CMD-SHIFT-ENTER"))
                  )),
      div(class="col-xs-6", htmlOutput("rmd_knitDoc"))
    )
  )
})

valsRmd <- reactiveValues(knit = 0)

knitIt <- function(text) knitr::knit2html(text = text, quiet = TRUE, options=c("mathjax", "base64_images"),
                                          stylesheet = "../base/www/rmarkdown.css") %>% HTML

# rmarkdown requires pandoc install
# knitIt <- function(text) rmarkdown::render(input = tmpfile(text))

knitIt2 <- function(text) paste(knitr::knit2html(text = text, fragment.only = TRUE, quiet = TRUE),
                                "<script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>",
                                "<script>MathJax.Hub.Typeset();</script>", sep = '\n') %>% HTML

observe({
  input$runKeyRmd
  if(!is.null(input$evalRmd)) isolate(valsRmd$knit <- valsRmd$knit + 1)
})

output$rmd_knitDoc <- renderUI({
  if(valsRmd$knit == 1) return()

  isolate({
    if(!running_local) {
      return(HTML("<h2>Rmd file is not evaluated when running Radiant on a server</h2>"))
    }
    if(input$rmd_report != "") {
      withProgress(message = 'Knitting report', value = 0, {
        ifelse(is.null(input$rmd_selection) || input$rmd_selection == "", return(knitIt2(input$rmd_report)),
          return(knitIt2(input$rmd_selection)))
      })
    }
  })
})

output$saveHTML <- downloadHandler(
  filename = function() {"report.html"},
  content = function(file) {
    if(running_local) {
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
      input$rmd_report %>% cat(.,file=file,sep="\n")
    })
  }
)

observe({
  # loading rmd report from disk
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

  # deprecate in favor of update_report below

  cmd <- paste0("result <- ", sub('list',fun_name, deparse(inp, control = c("keepNA"), width.cutoff = 500L)),
                collapse="\n")
  cmd <- paste0(cmd, "\n", sum_name,"(result)\n", plots_name,"(result)")
  if(xcmd != "") cmd <- paste0(cmd, "\n", xcmd)
  cmd <- paste0("\n```{r fig.width=",fig.width,", fig.height=",fig.height,"}\n",cmd,"\n```\n")
  update_report_fun(cmd)
}

# test for update_report
# library(dplyr)
# c("dataset", "sm_var", "sm_comp_value", "sm_alternative", "sm_sig_level") %>%
#   setNames(as.list(.),.) -> base_sm_list
# # removing elements that are NA!!
# base_sm_list$sm_sig_level <- NA
# base_sm_list <- base_sm_list[!is.na(base_sm_list)]
# deparse(base_sm_list, control = c("keepNA"))
# update_report(inp = base_sm_list, fun_name = "single_mean")
# update_report(inp = base_sm_list, fun_name = "single_mean", pre = "",
#                outputs = c(), figs = FALSE)

# updating the report when called
update_report <- function(inp, fun_name, pre_cmd = "result <- ",
                          outputs = c("summary", "plot"),
                          figs = TRUE, fig.width = 7, fig.height = 7, xcmd = "") {

  cmd <- paste0(pre_cmd, sub('list',fun_name, deparse(inp, control = c("keepNA"), width.cutoff = 500L)))
  for(i in outputs)
    cmd <- paste0(cmd, "\n", i, "(result)")

  if(xcmd != "")
    cmd <- paste0(cmd, "\n", xcmd)

  if(figs)
    cmd <- paste0("\n```{r fig.width=",fig.width,", fig.height=",fig.height,"}\n",cmd,"\n```\n")
  else
    cmd <- paste0("\n```{r}\n",cmd,"\n```\n")

  update_report_fun(cmd)
}

update_report2 <- function(inp_main = "", fun_name = "", inp_out = list("",""), pre_cmd = "result <- ",
                          outputs = c("summary", "plot"),
                          figs = TRUE, fig.width = 7, fig.height = 7, xcmd = "") {

  cmd <- ""
  if(inp_main[1] != "") {
    cmd <- deparse(inp_main, control = c("keepNA"), width.cutoff = 500L) %>%
             sub("list", fun_name, .) %>%
             paste0(pre_cmd, .)
  }

  lout <- length(outputs)
  if(lout > 0) {
    for(i in 1:lout) {
      if(inp_out[i] != "") {
        cmd <- deparse(inp_out[[i]], control = c("keepNA"), width.cutoff = 500L) %>%
                 sub("list\\(", paste0(outputs[i], "\\(result, "), .) %>%
                 paste0(cmd, "\n", .)
      } else {
        cmd <- paste0(cmd, "\n", outputs[i], "(result)")
      }
    }
  }

  if(xcmd != "") cmd <- paste0(cmd, "\n", xcmd)

  if(figs)
    cmd <- paste0("\n```{r fig.width=",fig.width,", fig.height=",fig.height,"}\n",cmd,"\n```\n")
  else
    cmd <- paste0("\n```{r}\n",cmd,"\n```\n")

  update_report_fun(cmd)
}

update_report_fun <- function(cmd) {

  if(!is.null(input$manualPaste) && input$manualPaste) {
    os_type <- Sys.info()["sysname"]
    if (os_type == 'Windows') {
      cat(cmd, file = "clipboard")
    } else if (os_type == "Darwin") {
      cat(cmd, file = pipe("pbcopy"))
    } else if (os_type == "Linux") {
      cat("Clipboard not supported on linux")
      # nothing yet
    }
    # by setting cmd to "" nothing is added to the report
    cmd <- ""
  }

  if(cmd != "") {
    if(is.null(input$rmd_report)) {
      if(is.null(r_state$rmd_report)) {
        r_state$rmd_report <<- cmd
      } else {
        r_state$rmd_report <<- paste0(r_state$rmd_report,"\n",cmd)
      }
    } else {
      updateAceEditor(session, "rmd_report",
                      value = paste0(input$rmd_report,"\n",cmd))
    }
  }

  # move to the report panel
  updateTabsetPanel(session, "nav_radiant", selected = "Report")
  # updateTabsetPanel(session, "nav_radiant", selected = "transform")
  # updateTabsetPanel(session, "nav_radiant", selected = "Regression")
  # updateTabsetPanel(session, "nav_radiant", selected = "GLM")
  # updateTabsetPanel(session, "nav_radiant", selected = "GLM")
  # updateTabsetPanel(session, "tabs_glm_reg", selected = "plot_glm_reg")
  # updateTabsetPanel(session, "nav_radiant", selected = "Transform")
  # updateTabsetPanel(session, "nav_radiant", selected = "Data")
  # updateTabsetPanel(session, "datatabs", selected = "Transform")
  # updateTabsetPanel(session, "nav_radiant", selected = "Transform")
}


################################################################
# Run R-code within Radiant using the shinyAce editor
################################################################
r_example <- "# to get the currently active data
dat <- .getdata()

# show the first observations
head(dat)

# to access a specific dataset by name
dat <- r_data[['diamonds']]

# add a variable to the data used by Radiant
r_data[['diamonds']]$log.price <- log(dat$price)
dat <- r_data[['diamonds']]

# show the first observations
head(dat)

# plotting a figure
plot(1:10)

# run a regression
reg <- lm(price ~ carat + clarity, data = dat)
summary(reg)
"

output$rcode <- renderUI({
  div(class="row", div(class="col-xs-6",
    aceEditor("r_code", mode="r", selectionId = "r_code_selection", value=state_init("r_code",r_example),
              hotkeys=list(runKeyCode=list(win="Ctrl-R|Ctrl-Shift-Enter", mac="CMD-ENTER|CMD-SHIFT-ENTER"))
              ),
    actionButton("rEval", "Run"),
    downloadButton('saveCode', 'Save R-code'), tags$br(), tags$br(),
    fileInput('loadCode', 'Load R-code', multiple=FALSE)
    #, fileInput('sourceCode', 'Source R-code', multiple=TRUE)
  ),
  div(class="col-xs-6", htmlOutput("rCodeEval"))
  )
})

valsCode <- reactiveValues(code = 0)

observe({
  input$runKeyCode
  if(!is.null(input$rEval)) isolate(valsCode$code <- valsCode$code + 1)
})

output$rCodeEval <- renderPrint({
  if(valsCode$code == 1) return()
  isolate({
    if(running_local) {
      if(is.null(input$r_code_selection) || input$r_code_selection == "") {
        r_code <- input$r_code
      } else {
        r_code <- input$r_code_selection
      }

      r_output <- paste0("```{r cache = FALSE, echo = TRUE}\n",r_code,"\n```")
      return(HTML(paste(knitr::knit2html(text = r_output, fragment.only = TRUE, quiet = TRUE),
             '<script>', 'MathJax.Hub.Typeset();', '</script>', sep = '\n')))
    } else {
      return(HTML("<h2>Code is not evaluated when running Radiant on a server</h2>"))
    }
  })
})

output$saveCode <- downloadHandler(
  filename = function() {"rcode.R"},
  content = function(file) {
    isolate({
      cat(input$r_code,file=file,sep="\n")
    })
  }
)

# loading r-code from disk
observe({
  inFile <- input$loadCode
  if(!is.null(inFile)) {
    isolate({
      paste0(readLines(inFile$datapath), collapse = "\n") %>%
        updateAceEditor(session, "r_code", value = .)
    })
  }
})

# source r-code from disk
# observe({
#   inFile <- input$sourceCode
#   isolate({
#     if(!is.null(inFile)) source(inFile$datapath)
#   })
# })
