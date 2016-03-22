# rm -rf  ~/gh/radiant/inst/base/tools/help/figures/
rsync -av ~/gh/radiant_gh-pages/base/app/  ~/gh/radiant/inst/base/tools/help/
rsync -av ~/gh/radiant_gh-pages/base/figures/*  ~/gh/radiant/inst/base/tools/help/figures/

rsync -av ~/gh/radiant_gh-pages/quant/app/  ~/gh/radiant/inst/quant/tools/help/
rsync -av ~/gh/radiant_gh-pages/quant/figures_quant/  ~/gh/radiant/inst/quant/tools/help/figures/

rsync -av ~/gh/radiant_gh-pages/marketing/app/  ~/gh/radiant/inst/marketing/tools/help/
rsync -av ~/gh/radiant_gh-pages/marketing/figures_marketing/  ~/gh/radiant/inst/marketing/tools/help/figures/

rsync -av ~/gh/radiant_gh-pages/analytics/app/  ~/gh/radiant/inst/analytics/tools/help/
rsync -av ~/gh/radiant_gh-pages/analytics/figures_analytics/  ~/gh/radiant/inst/analytics/tools/help/figures/


# rsync -nav ~/gh/radiant_gh-pages/base/app/  ~/gh/radiant/inst/base/tools/help/
# rsync -nav ~/gh/radiant_gh-pages/base/figures/  ~/gh/radiant/inst/base/tools/help/figures/

# rsync -nav ~/gh/radiant_gh-pages/quant/app/  ~/gh/radiant/inst/quant/tools/help/
# rsync -nav ~/gh/radiant_gh-pages/quant/figures_quant/  ~/gh/radiant/inst/quant/tools/help/figures/
#
# rsync -nav ~/gh/radiant_gh-pages/marketing/app/  ~/gh/radiant/inst/marketing/tools/help/
# rsync -nav ~/gh/radiant_gh-pages/marketing/figures_marketing/  ~/gh/radiant/inst/marketing/tools/help/figures/
#
# rsync -nav ~/gh/radiant_gh-pages/analytics/app/  ~/gh/radiant/inst/analytics/tools/help/
# rsync -nav ~/gh/radiant_gh-pages/analytics/figures_analytics/  ~/gh/radiant/inst/analytics/tools/help/figures/
#
