# sourcing from radiant base
source('../base/global.R', local = TRUE)

# add functions below that may not be needed in other apps
if(!"package:radiant" %in% search()) {
	addResourcePath("figures_marketing", "tools/help/figures/")
	addResourcePath("figures_quant", "../quant/tools/help/figures/")
	# approach to use for shinyapps.io - must have radiant package installed
	# addResourcePath("figures_quant", system.file("quant/tools/help/figures/", package="radiant"))
}
