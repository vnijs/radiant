# path to use for local and server use
path <- ".."

# path to use for shinyapps.io
# path <- system.file(package = "radiant")

# if radiant is not installed revert to path in the inst directory
if(path == "") path <- ".."

# sourcing from radiant base
source(file.path(path,'/base/global.R'), local = TRUE)

# add functions below that may not be needed in other apps

if(!"package:radiant" %in% search())
	addResourcePath("figures_quant", file.path(path,"/quant/tools/help/figures/"))
