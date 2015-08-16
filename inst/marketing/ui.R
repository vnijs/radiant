source(file.path(r_path,"quant/quant_ui.R"), encoding = "UTF-8", local = TRUE)

marketing_ui <- tagList(
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
  )
)

shinyUI(
  do.call(navbarPage, c("Marketing Research", nav_ui, quant_ui, marketing_ui, shared_ui))
)

## see http://www.inside-r.org/questions/adding-divider-navbarmenu-shinyo
# tabPanel(HTML("<li class='divider'></li><li class='dropdown-header'>Test</li>")),
# tabPanel(HTML("<li class='divider'></li>")),
# %>% sub("<li>div class=\"tab-pane\" data-value=\"Regression-divider-1\" id=\".{5,10}\"></div>","",.)
# )) %>% sub("<div class=\"tab-pane\" data-value=\"Regression-divider-1\" id=\".{5,10}\"></div>","",.)
# <li>
#   <a href="#tab-8114-3" data-toggle="tab" data-value="Regression-divider-1">Regression-divider-1</a>
# </li>
# <div class="tab-pane" data-value="Regression-divider-1" id="tab-7605-3"></div>
