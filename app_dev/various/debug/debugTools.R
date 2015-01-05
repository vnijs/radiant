##  For source'ing in server.R, and using in ui.R via uiOutput("debugTools")

assign("%&%",  function(a, b) paste(a, b, sep = ""))
catn <- function(...) cat(..., "\n")
wasClicked <- function(button) {
  if(exists("input") && !is.null(button)) {
    if(button > 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

output$evaluatedOutputR = renderText({
  if(wasClicked(input$evalButtonR)) {
    evalString = isolate(input$evalStringR)
  capture.output(eval(parse(text=evalString)))
  ## You have to isolate input$evalStringR; otherwise each character typed calls this callback.
  ## The following might be useful later for up-arrowing through past expressions.
  #   if(is.null(rValues$evalStringHistory))
  #     rValues$evalStringHistory = character(0)
  #  rValues$evalStringHistory = c(rValues$evalStringHistory, evalString)
  }
})

outputPreambleJS <<- 'window.Shiny.shinyapp.$bindings.'
# EXAMPLE:  window.Shiny.shinyapp.$bindings.selTxt.firstChild.nodeValue
inputPreambleJS <<- 'window.Shiny.shinyapp.$inputValues.'
wrapperToGetKeys <<- function(x) "Object.keys(" %&% x %&% ")"
observerPreambleToggles = observe({
  input$prependInputPreambleToggle
  input$prependOutputPreambleToggle
  try({
    evalString = isolate(input$evalStringJS)
    if(wasClicked(input$prependInputPreambleToggle)) {
      if(substr(evalString, 1, nchar(inputPreambleJS)) != inputPreambleJS)
        evalString = paste0(inputPreambleJS, evalString)
    }
    else ## Remove inputPreambleJS
      evalString = gsub(inputPreambleJS, "", evalString, fixed=TRUE)

    if(wasClicked(input$prependOutputPreambleToggle)) {
      if(substr(evalString, 1, nchar(outputPreambleJS)) != outputPreambleJS)
        evalString = paste0(outputPreambleJS, evalString)
    }
    else ## Remove outputPreambleJS
      evalString = gsub(outputPreambleJS, "", evalString, fixed=TRUE)
    isolate( { rValues$evalStringJS = evalString } )
    catn("Responding to preamble toggles, evalString=", evalString)
    updateTextInput(thisSession, "evalStringJS", label="", value=rValues$evalStringJS)
    # You need to specify the label arg too. The default, NULL, doesn't cut it.
  })
})

#output$evaluatedOutputJS = renderText({
  #shinyalert("JS output is in a popup alert window, if there was no error. Otherwise nothing happens")
# }
# )

output$JSevaluation = renderUI({
  if(wasClicked(input$evalButtonJS) ) {
    evalString = gsub('"', "'", isolate(input$evalStringJS)) # replace all DQ with SQ.
    div(list(tags$script(
      # 'alert(', '"HERE IS JS"', ')'     # THIS WORKS!
      # 'alert(eval(', '"1+2"', '))'       # THIS WORKS!
      paste0(
        'alert(eval("', evalString, '"))'       # THIS WORKS!
      )
    )))
  }
  # TRY THIS SOME TIME, to avoid creating an alert window for the JS output:
  #document.getElementById("demo").innerHTML = ... ;
})

output$shiny.trace.text = renderText({
  eval(options(shiny.trace=input$traceCheckbox), envir = .GlobalEnv);
  cat("shiny.trace: ", options("shiny.trace")[[1]], "\n")
  if( options("shiny.trace")[[1]] != input$traceCheckbox)
    cat('Error: options("shiny.trace")[[1]] should equal input$traceCheckbox', "\n");
  paste("trace=", input$traceCheckbox)
})   #### OK this works now.


output$debugTools = renderUI({
  div(style="background:darkGrey",
      singleton(tags$script(paste(
        "outputPreamble = '", outputPreambleJS, "';")))
      ,
      checkboxInput(inputId='debugToolsCheckbox', value=FALSE,
                    label=em(strong("Debugging aids"))),
      conditionalPanel('input.debugToolsCheckbox',
                      tag("table", list(
                        tag("tr",
                            list(
                              tag("TD",
                                  list(width=120, style="color: blue",
                                       checkboxInput(inputId="traceCheckbox",
                                                     value=FALSE,
                                                     label=textOutput("shiny.trace.text")
                                       ))) ,
                              tag("TD", list(HTML(paste0(rep("&nbsp;",15), collapse="")))),
                              tag("TD",
                                  list(
                                    actionButton("evalButtonJS",
                                                    HTML("<font color='red'> evaluate JS</font>"))
                                       %>% tagAppendAttributes(
                                         style="display: flex; justify-content:flex-end;")
                                    # Cool! Too bad it doesn't work.
                                  , checkboxInput(inputId="prependOutputPreambleToggle",
                                                  value=FALSE,
                                                  label="prependOutputPreambleToggle")
                                  , checkboxInput(inputId="prependInputPreambleToggle",
                                                  value=FALSE,
                                                  label="prependInputPreambleToggle")
                                  )
                              ),
                              tag("TD",
                                  list(width=10, tags$textarea(id = "evalStringJS",
                                                               value=""))),
                              tag("TD", list(HTML(paste0(rep("&nbsp;",15), collapse="")))),

                              tag("TD",
                                  list(actionButton("evalButtonR",
                                                    HTML("<font color='red'> evaluate R</font>"))
                                       , HTML(paste0(rep("&nbsp;",25), collapse=""))
                                       , br()
                                       , br()
                                  )
                              ),
                              tag("TD", list(style="color:red", HTML("&rarr;"))),
                              tag("TD",
                                  list(width=10, tags$textarea(id = "evalStringR",
                                                               value=""))),
                              tag("TD",
                                  list(width=800,
                                       style='text-align:"right"; color:white',
                                       uiOutput("evaluatedOutputR"))),
                              uiOutput('JSevaluation')
                            )
                        )
                      )
                      )
      )
      )
})

