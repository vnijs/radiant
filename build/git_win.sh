#!/bin/sh
# run from git terminal
cd ~/Desktop/GitHub
#git clone git@github.com:smartinsightsfromdata/rpivotTable.git
#git clone git@github.com:trestletech/shinyAce.git
cd radiant
git pull
cd ../shinyAce
git pull
cd ../rpivotTable
git pull
cd ../DT
git pull

## check DT_vnijs/README.md to merge changes in DT into DT_vnijs
