cd %OHRRPGCE%

del tmp\*.???

copy game.exe tmp\wander.exe
copy wander.rpg tmp
copy wander.ico tmp
copy wanderp.hss tmp
copy readme-wander.txt tmp
copy LICENSE-binary.txt tmp

del distrib\wander.zip

cd tmp
..\support\pkzip ..\distrib\wander.zip *.*
