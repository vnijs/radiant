################################################################
# Run R-code within Radiant using the shinyAce editor
################################################################
r_example <- "## get the active dataset and show the first few observations
.getdata() %>% head

## access a specific dataset by name
r_data[['diamonds']] %>% select(price, clarity) %>% head

## add a variable to the diamonds data
dat <- r_data[['diamonds']]
dat$log_price <- log(dat$price)

## show the first observations
dat %>% select(price, log_price) %>% head

## create a histogram of prices
dat %>% ggplot(aes(x = price)) + geom_histogram()

## and a histogram of log-prices
dat %>% ggplot(aes(x = log_price)) + geom_histogram()

## open help in the R-studio viewer from Radiant
help(package = 'radiant')

## if you are familiar with Shiny you can call reactives here
## for example, if you just transformed some variables in Data > Transform
## you can call the transform_main reacive to see the latest result
## this can very useful for debugging
transform_main() %>% head"

output$rcode <- renderUI({
  tagList(
    with(tags,
      table(
            td(help_modal('Code','code_help',
                       inclMD(file.path(r_path,"base/tools/help/code.md")))),
            td(HTML("&nbsp;&nbsp;")),
            td(actionButton("rEval", "Run code")),
            td(downloadButton("save2HTML", "Save HTML")),
            td(downloadButton('saveCode', 'Save')),
            td(HTML("<div class='form-group shiny-input-container'>
                <input id='load_code' name='load_code' type='file' accept='.r,.R'/>
              </div>"))
            #, td(fileInput('sourceCode', 'Source R-code', multiple=TRUE))
      )
    ),

    shinyAce::aceEditor("rmd_code", mode = "r",
      vimKeyBinding = ifelse (is.null(r_data$vim_keys), FALSE, r_data$vim_keys),
      height="auto",
      selectionId = "rmd_code_selection",
      value = state_init("rmd_code",r_example),
      hotkeys = list(runKeyCode = list(win ="CTRL-ENTER", mac ="CMD-ENTER"))),
    htmlOutput("rmd_code_output")
  )
})

valsCode <- reactiveValues(code = 0)

observe({
  input$runKeyCode
  if (!is.null(input$rEval)) isolate(valsCode$code <- valsCode$code + 1)
})

output$rmd_code_output <- renderUI({

  if (valsCode$code == 1) return()
  isolate({
    if (r_local) {
      rmd_code <- if (is_empty(input$rmd_code_selection)) input$rmd_code
                  else input$rmd_code_selection

      paste0("```{r cache = FALSE, echo = TRUE}\n", rmd_code ,"\n```") %>%
        ## need r_env so changes are reflected in the shiny environment
        ## does mean user can mess things up pretty good so not a good idea on a server
        knitr::knit2html(text = ., fragment.only = TRUE, quiet = TRUE, envir = r_env) %>%
        HTML
    } else {
      HTML("<h2>Code is not evaluated when running Radiant on a server</h2>")
    }
  })
})

output$saveCode <- downloadHandler(
  filename = function() {"rcode.R"},
  content = function(file) {
    isolate({
      cat(input$rmd_code,file=file,sep="\n")
    })
  }
)

## loading r-code from disk
observe({
  inFile <- input$load_code
  if (!is.null(inFile)) {
    isolate({
      paste0(readLines(inFile$datapath), collapse = "\n") %>%
        shinyAce::updateAceEditor(session, "rmd_code", value = .)
    })
  }
})

output$save2HTML <- downloadHandler(
  filename = function() {"code.html"},
  content = function(file) {
    if (r_local) {
      isolate({
        withProgress(message = "Knitting report", value = 0, {
          rmd_code <- if (is_empty(input$rmd_code_selection)) input$rmd_code
                      else input$rmd_code_selection
          paste0("```{r cache = FALSE, echo = TRUE}\n", rmd_code ,"\n```") %>%
          knitIt %>% cat(file=file,sep="\n")
          # knitr::knit2html(text = ., fragment.only = TRUE, quiet = TRUE, envir = r_env) %>%
          # HTML %>% cat(file=file,sep="\n")
        })
      })
    }
  }
)
