cd %OHRRPGCE%

REM Compress
CALL compress.bat

REM copy distrib files
copy whatsnew.txt distrib
copy custom.txt distrib
copy wh_demo.txt distrib
copy howto.txt distrib
copy plotscripttutor.html distrib
copy plotdictionary.html distrib
copy hamsterspeak.html distrib
copy sample\*.rpg .

REM Create wh_demo package
copy game.exe wander.exe
pkzip distrib\wh_demo.zip wander.exe wander.rpg wh_demo.txt
del wander.exe

REM Create custom package
pkzip distrib\custom.zip game.exe custom.exe hspeak.exe unlump.exe
pkzip distrib\custom.zip ohrrpgce.mas ohrrpgce.fnt ohrrpgce.new custom.txt whatsnew.txt howto.txt
pkzip distrib\custom.zip sample.rpg npc_tag.rpg pstutor.rpg
pkzip distrib\custom.zip cleanup.bat plotscr.hsd scancode.hsi game.ico
pkzip distrib\custom.zip hamsterspeak.html plotscripttutor.html plotdictionary.html
pkzip distrib\custom.zip wanderp.hss wander.hsi
pkzip -P -r distrib\custom.zip import\*.*

REM Create ohrrpgce_play package
pkzip distrib\ohrrpgce_play.zip game.exe ohrrpgce.fnt game.txt game.ico

REM Create rpgfix package
pkzip distrib\rpgfix.zip rpgfix.exe lumpfix.exe fix.txt

REM Create hspeak-source packages
pkzip distrib\hspeak-eu-source.zip hspeak.ex hsspiffy.e
REM pkzip distrib\hspeak-qb-source.zip hspeak.bas allmodex.*
