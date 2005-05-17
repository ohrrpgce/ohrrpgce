cd %OHRRPGCE%

del tmp\*.???

ECHO Packaging binary distribution of CUSTOM

copy game.exe tmp
copy custom.exe tmp
copy hspeak.exe tmp
copy unlump.exe tmp
copy ohrrpgce.mas tmp
copy ohrrpgce.fnt tmp
copy ohrrpgce.new tmp
copy README-custom.txt tmp
copy LICENSE.txt tmp
copy LICENSE-binary.txt tmp
copy whatsnew.txt tmp
copy sample\sample.rpg tmp
copy sample\npc_tag.rpg tmp
copy sample\pstutor.rpg tmp
copy plotscr.hsd tmp
copy scancode.hsi tmp
copy game.ico tmp
copy custom.ico tmp
copy hamsterspeak.html tmp
copy plotscripttutor.html tmp
copy plotdictionary.html tmp

del distrib\custom.zip

cd tmp
..\support\pkzip ..\distrib\custom.zip *.*
cd ..
support\pkzip -p -r distrib\custom.zip import

