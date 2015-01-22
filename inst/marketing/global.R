# sourcing from radiant base
source('../base/global.R', local = TRUE)

# add functions below that may not be needed in other apps
if(!"package:radiant" %in% search())
	addResourcePath("figures_marketing", "../marketing/tools/help/figures/")
	addResourcePath("figures_quant", "../quant/tools/help/figures/")
