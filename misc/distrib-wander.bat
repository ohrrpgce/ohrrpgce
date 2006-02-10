cd %OHRRPGCE%

del tmp\*.???

copy game.exe tmp\wander.exe
copy wander.rpg tmp
copy misc\wander.ico tmp
copy misc\wanderp.hss tmp
copy misc\readme-wander.txt tmp
copy LICENSE-binary.txt tmp

del distrib\wander.zip

cd tmp
..\support\pkzip ..\distrib\wander.zip *.*
