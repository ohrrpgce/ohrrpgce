cd %OHRRPGCE%

del tmp\*.???

copy game.exe tmp\pushpush.exe
copy pushpush.rpg tmp
copy misc\pushpush.ico tmp
copy misc\pushpush.hss tmp
copy misc\readme-pushpush.txt tmp
copy LICENSE-binary.txt tmp

del distrib\pushpush.zip

cd tmp
..\support\pkzip ..\distrib\pushpush.zip *.*
