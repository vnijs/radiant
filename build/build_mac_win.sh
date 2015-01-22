#!/bin/bash

dirsource=../radiant_miniCRAN/src/contrib/
dirmac=../radiant_miniCRAN/bin/macosx/contrib/3.1/
dirmac_mav=../radiant_miniCRAN/bin/macosx/mavericks/contrib/3.1/
dirwin=../radiant_miniCRAN/bin/windows/contrib/3.1/
#
# R --save < build/build_mac_source.R 2>&1

# mv ../*.tar.gz $dirsource
# cp ../*.tgz $dirmac
# mv ../*.tgz $dirmac_mav

mv ../*.zip $dirwin

# library(tools)
# write_PACKAGES('../radiant_miniCRAN/bin/macosx/contrib/3.1/',type = 'mac.binary')
# write_PACKAGES('../radiant_miniCRAN/bin/macosx/mavericks/contrib/3.1/',type = 'mac.binary')
# write_PACKAGES('../radiant_miniCRAN/bin/windows/contrib/3.1/',type = 'win.binary')
# write_PACKAGES('../radiant_miniCRAN/src/contrib/',type = 'source')

#
# R --save < writePackages.R 2>&1
# cd ../radiant_miniCRAN
# git push
# cd ../radiant_dev
#
