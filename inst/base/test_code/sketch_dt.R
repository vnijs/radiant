# initial for explore with DT
  # if (all(cvars == "")) {
  # } else if (length(fun) == 1) {
  #   pfun <- make_funs(fun) %>% names
  #   sketch = shiny::withTags(table(
  #     thead(
  #       tr(
  #         th(" ", colspan = length(cvars)),
  #         lapply(pfun, th, colspan = length(vars), class = "text-center")
  #       ),
  #       tr(
  #         lapply(cvars, th), lapply(vars, th)
  #       )
  #     )
  #   ))
  # } else if (length(vars) == 1) {
  #   pfun <- make_funs(fun) %>% names
  #   sketch = shiny::withTags(table(
  #     thead(
  #       tr(
  #         th(" ", colspan = length(cvars)),
  #         lapply(vars, th, colspan = length(pfun), class = "text-center")
  #       ),
  #       tr(
  #         lapply(cvars, th), lapply(pfun, th)
  #       )
  #     )
  #   ))
  # } else {
  #   pfun <- cn %>% matrix(ncol = length(vars), byrow = TRUE) %>% .[,1]  %>%
  #           gsub(paste0(vars[1],"_"),"",.)
  #   sketch = shiny::withTags(table(
  #     thead(
  #       tr(
  #         th(" ", colspan = length(cvars)),
  #         lapply(pfun, th, colspan = length(vars), class = "text-center")
  #       ),
  #       tr(
  #         lapply(cn_all, th)
  #       )
  #     )
  #   ))
  # }
