cd %OHRRPGCE%

del tmp\*.???

copy game.exe tmp\pushpush.exe
copy pushpush.rpg tmp
copy pushpush.ico tmp
copy pushpush.hss tmp
copy readme-pushpush.txt tmp
copy LICENSE-binary.txt tmp

del distrib\pushpush.zip

cd tmp
..\support\pkzip ..\distrib\pushpush.zip *.*