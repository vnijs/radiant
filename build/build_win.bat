R --save < build_win.R 2>&1

del ../radiant_miniCRAN/bin/windows/contrib/3.1/radiant*
mv ../*.zip ../radiant_miniCRAN/bin/windows/contrib/3.1/
