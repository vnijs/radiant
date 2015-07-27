# if base is available in inst/ use it, else use installed radiant (i.e., for shinyapps.io)
bp <- if (file.exists("../base"))  ".." else system.file(package = "radiant")
if (bp == "") stop("Radiant base app not found")

# sourcing from radiant base, note that path is set in base/global.R
# source(file.path(bp,"base/global.R"), encoding = r_encoding, local = TRUE)
source(file.path(bp,"base/global.R"), encoding = "UTF-8", local = TRUE)
rm(bp)

addResourcePath("figures_quant", "tools/help/figures/")

quant_ui <- tagList(
  navbarMenu("Sample",
    tabPanel("Sampling", uiOutput("sampling")),
    tabPanel("Sample size", uiOutput("sample_size"))
  ),

  navbarMenu("Base",
    tabPanel("Single mean", uiOutput("single_mean")),
    tabPanel("Compare means", uiOutput("compare_means")),
    tabPanel("Single proportion", uiOutput("single_prop")),
    tabPanel("Compare proportions", uiOutput("compare_props")),
    tabPanel("Cross-tabs", uiOutput("cross_tabs"))
  ),

  navbarMenu("Regression",
    tabPanel("Correlation", uiOutput("correlation")),
    tabPanel("Linear (OLS)", uiOutput("regression")),
    tabPanel("GLM", uiOutput("glm_reg"))
  ),

  navbarMenu("Decide",
    tabPanel("Decision tree", uiOutput("dtree")),
    tabPanel("Simulate", uiOutput("simulater"))
  )
)
