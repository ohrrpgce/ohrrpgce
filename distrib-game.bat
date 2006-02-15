REM Only works in Windows 98. Windows XP screws up long file names for some reason
cd %OHRRPGCE%

del tmp\*.???

ECHO Packaging binary distribution of GAME

copy game.exe tmp
copy ohrrpgce.fnt tmp
copy README-game.txt tmp
copy LICENSE-binary.txt tmp

del distrib\ohrrpgce_play.zip

cd tmp
..\support\pkzip ..\distrib\ohrrpgce_play.zip *.*
cd ..
