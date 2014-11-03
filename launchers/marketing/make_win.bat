set R="-"
IF exist "C:\Program Files\R\R-3.1.1\bin\Rscript.exe" (set R="C:\Program Files\R\R-3.1.1\bin\Rscript.exe")
IF exist "C:\Program Files\R\R-3.1.1\bin\x64\Rscript.exe" (set R="C:\Program Files\R\R-3.1.1\bin\x64\Rscript.exe")

if %R% == "-" goto R_not_found else goto R_found

:R_found
echo R was found in "%R%"
%R% ../make/make_win_dev.R "%CD%" "marketing" "4475"
goto end

:R_not_found
echo R is not installed or is not installed in the expected location
echo Make sure R is installed on your system
goto end

:end
pause

