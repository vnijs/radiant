#!/bin/bash

cd ~/Desktop/GitHub/radiant_dev

dirsource=../radiant_miniCRAN/src/contrib/
dirmac=../radiant_miniCRAN/bin/macosx/contrib/3.1/
dirmac_mav=../radiant_miniCRAN/bin/macosx/mavericks/contrib/3.1/
dirwin=../radiant_miniCRAN/bin/windows/contrib/3.1/

R --save < build/build_mac_source.R 2>&1
mv ../*.tar.gz $dirsource
cp ../*.tgz $dirmac
mv ../*.tgz $dirmac_mav
mv ../*.zip $dirwin

R --save < build/write_package_files.R 2>&1
cd ../radiant_miniCRAN
git add .
git commit -m "radiant package updates 1/23"
git push
cd ../radiant_dev

