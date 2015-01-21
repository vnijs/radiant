#!/bin/bash

dirsource ../radiant_miniCRAN/src/contrib/
dirmac ../radiant_miniCRAN/bin/macosx/contrib/3.1/
dirmac_mav ../radiant_miniCRAN/bin/macosx/mavericks/contrib/3.1/
dirwin ../radiant_miniCRAN/bin/windows/contrib/3.1/

R64 --save < build_mac_source.R 2>&1
#
# mv ../*.tar.gz $dirsource
# cp ../*.tgz $dirmac
# mv ../*.tgz $dirmac_mav
#
# while true; do
# 	read -p "Have you compiled the Windows binary?" yn
# 	  case $yn in
# 		[Yy]* ) break;;
# 		* ) echo "I'll have to wait till that is done.";;
#     esac
# done
#
# mv ../*.zip $dirwin
#
# R64 --save < writePackages.R 2>&1
# cd ../radiant_miniCRAN
# git push
# cd ../radiant_dev
#
