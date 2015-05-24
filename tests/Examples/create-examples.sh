## based on http://r.789695.n4.nabble.com/timings-for-examples-in-R-CMD-check-td4696443.html
cd ~/gh/
rm -f radiant/tests/Examples/radiant-Ex.Rout.save
R CMD build radiant
R CMD check radiant*.tar.gz
mv radiant.Rcheck/radiant-Ex.Rout radiant/tests/Examples/radiant-Ex.Rout.save
rm -rf radiant.Rcheck
cd ~/gh/radiant
