@echo off
if not exist notate.* goto done
echo -------------------------------
echo Cleaning up stuff. Please wait.
echo -------------------------------
md import
move *.bmp import >nul
move *.bam import >nul
move *.not import >nul
move ibank.ibk import >nul
move notate.* import >nul
move playbam.exe notate >nul
echo.
:done
echo ----------------------------------------
echo Your O.H.R.RPG.C.E directorys are clean.
echo ----------------------------------------