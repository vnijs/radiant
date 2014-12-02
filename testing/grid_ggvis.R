lb <- linked_brush(keys = 1:nrow(mtcars), "red")

# Change the colour of the points
mtcars %>%
  ggvis(~disp, ~mpg) %>%
  layer_points(fill := lb$fill, size.brush := 400) %>%
  lb$input()

# Display one layer with all points and another layer with selected points
library(shiny)
mtcars %>%
  ggvis(~disp, ~mpg) %>%
  layer_points(size.brush := 400) %>%
  lb$input() %>%
  layer_points(fill := "red", data = reactive(mtcars[lb$selected(), ]))




mtc <- mtcars
mtc$id <- 1:nrow(mtc)  # Add an id column to use ask the key

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- mtc[mtc$id == x$id, ]
  paste0(names(row), ": ", format(row), collapse = "<br />")
}

mtc %>% ggvis(x = ~wt, y = ~mpg, key := ~id) %>%
  layer_points() %>%
  add_tooltip(all_values, "hover")








server <- function(input, output) {
  observe({
    xvar <- if (is.null(input$column)) "wt" else input$column
    xprop <- prop("x", as.name(xvar))
    mtcars %>% ggvis(xprop, ~mpg) %>% layer_points() %>% bind_shiny("plot1")
  })
}

ui <- fluidPage(
  selectInput("column", "Column for x", c("wt", "mpg", "disp", "hp")),
  ggvisOutput("plot1")
)

shinyApp(server = server, ui = ui)

dat <- mtcars
dat$vs %<>% as.factor
dat$am %<>% as.factor
xvar <- prop("x", as.symbol("vs"))
yvar <- prop("y", as.symbol("mpg"))
fillvar <- prop("fill", as.symbol("vs"))
dat %>% ggvis(x = xvar, y = yvar, fill = fillvar)
fillvar <- prop("fill", as.symbol("am"))
dat %>% ggvis(x = xvar, y = yvar, fill = fillvar)

dat <- diamonds
# xvar <- prop("x", as.symbol("cut"))
xvar <- prop("x", as.symbol("carat"))
yvar <- prop("y", as.symbol("price"))
fillvar <- prop("fill", as.symbol("cut"))

dat %>% ggvis(x = xvar, y = yvar)

dat %>% ggvis(x = xvar, y = yvar, fill = fillvar)
fillvar <- prop("fill", as.symbol("color"))
dat %>% ggvis(x = xvar, y = yvar, fill = fillvar)

dat %>% group_by(cut) %>% ggvis(x = xvar, y = yvar, fill = fillvar)

dat %>% group_by(color) %>% ggvis(x = xvar, y = yvar, fill = fillvar)

# mtcars %>% ggvis(x = xvar, y = yvar)
dat %>% ggvis(x = xvar, y = yvar, fill = fillvar)

dat %>% ggvis(x = xvar, y = yvar, fill = "")
dat %>% ggvis(x = xvar, fill = fillvar)
names(mtcars)



library(ggvis)
#simple summary brush tooltip
x_bar <- "x&#772;"
sigma_hat <- "&sigma;&#770;"

brushed_summary <- function(items, session, page_loc, ...) {
  if (nrow(items) == 0) return()

  items$key__ <- NULL
  lines <- Map(function(name, vals) {
    paste0(name, ": ",
           x_bar, " = ", round(mean(vals), 2), " ",
           sigma_hat, " = ", round(sd(vals), 2)
    )
  }, names(items), items)
  html <- paste(unlist(lines), collapse = "<br />\n")

  show_tooltip(session, page_loc$r + 5, page_loc$t, html)
}

# Scatter plot with brushing
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(size.brush := 400) %>%
  handle_brush(brushed_summary)

# Bar graph with brushing
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  scale_nominal("x", range = "width", padding = 0, points = FALSE) %>%
  layer_rects(y2 = 0, width = band(), fill.brush := "red") %>%
  handle_brush(brushed_summary)




library(ggvis)
set.seed(1233)
cocaine <- cocaine[sample(1:nrow(cocaine), 500), ]
cocaine$id <- seq_len(nrow(cocaine))

# adapted from ggvis > demo > apps > brush-linked

ui <- bootstrapPage(
  #   ggvisOutput("plot1"),
  #   ggvisOutput("plot2")
  htmlOutput("grid_ggvis")
)

server <- function(input, output, session) {

  lb <- linked_brush(keys = cocaine$id, "red")

  cocaine %>%
    ggvis(~weight, ~price, key := ~id) %>%
    layer_points(fill := lb$fill, fill.brush := "red", opacity := 0.3) %>%
    lb$input() %>%
    set_options(width = 300, height = 300) %>%
    bind_shiny("plot1") # Very important!


  # A subset of cocaine, of only the selected points
  selected <- lb$selected
  cocaine_selected <- reactive({
    cocaine[selected(), ]
  })

  cocaine %>%
    ggvis(~potency) %>%
    layer_histograms(width = 5, boundary = 0) %>%
    add_data(cocaine_selected) %>%
    layer_histograms(width = 5, boundary = 0, fill := "#dd3333") %>%
    set_options(width = 300, height = 300) %>%
    bind_shiny("plot2")

  output$grid_ggvis <- renderUI({

    p1 <- ggvisOutput("plot1")
    p2 <- ggvisOutput("plot2")

    # no graphs shown
    # HTML(paste0("<table><td>",p1,"</td><td>",p2,"</td></table>"))

    # clunky but seems to work
    html1 <- HTML("<table><td>")
    html2 <- HTML("</td><td>")
    html3 <- HTML("</td></table>")
    list(html1,p1,html2,p2,html3)
  })
}

shinyApp(ui = ui, server = server)
