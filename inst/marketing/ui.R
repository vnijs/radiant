shinyUI(
  navbarPage("Marketing Research", windowTitle = "Radiant", id = "nav_radiant",
             inverse = TRUE, collapsible = TRUE,

  tabPanel("Data", withMathJax(), uiOutput('ui_data')),

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

  # navbarMenu("Decide",
  #   tabPanel("Simulate", uiOutput("simulater"))
  # ),

  navbarMenu("Maps",
    tabPanel("(Dis)similarity", uiOutput("mds")),
    tabPanel("Attributes", uiOutput("pmap"))
  ),

  navbarMenu("Factor",
    tabPanel("Pre-factor", uiOutput("pre_factor")),
    tabPanel("Factor", uiOutput("full_factor"))
  ),

  navbarMenu("Cluster",
    tabPanel("Hierarchical", uiOutput("hier_clus")),
    tabPanel("K-means", uiOutput("kmeans_clus"))
  ),

  navbarMenu("Conjoint",
    tabPanel("Create profiles", uiOutput("conjoint_profiles")),
    tabPanel("Conjoint", uiOutput("conjoint"))
  ),

  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),

  tabPanel("Quit", uiOutput("savequit")),

  navbarMenu("Help",
    tabPanel("Help", uiOutput("help_marketing")),
    tabPanel("Videos", uiOutput("help_videos")),
    tabPanel("About", uiOutput("help_about"))
  ),

  tags$head(tags$script(src="js/session.js"),
            tags$link(rel="shortcut icon", href="imgs/icon.png"))
))

## see http://www.inside-r.org/questions/adding-divider-navbarmenu-shinyo
# tabPanel(HTML("<li class='divider'></li><li class='dropdown-header'>Test</li>")),
# tabPanel(HTML("<li class='divider'></li>")),
# %>% sub("<li>div class=\"tab-pane\" data-value=\"Regression-divider-1\" id=\".{5,10}\"></div>","",.)
# )) %>% sub("<div class=\"tab-pane\" data-value=\"Regression-divider-1\" id=\".{5,10}\"></div>","",.)
# <li>
#   <a href="#tab-8114-3" data-toggle="tab" data-value="Regression-divider-1">Regression-divider-1</a>
# </li>
# <div class="tab-pane" data-value="Regression-divider-1" id="tab-7605-3"></div>
