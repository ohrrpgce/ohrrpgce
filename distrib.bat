cd %OHRRPGCE%

REM Compress
CALL compress.bat

REM copy distrib files
copy LICENSE.txt distrib
copy whatsnew.txt distrib
copy custom.txt distrib
copy wandering_hamster_readme.txt distrib
copy wandering_hamster_license.txt distrib
copy docs\plotscripttutor.html distrib
copy docs\plotdictionary.html distrib
copy docs\hamsterspeak.html distrib
copy sample\*.rpg .

REM Create wh_demo package
copy game.exe wander.exe
pkzip distrib\wh_demo.zip wander.exe wander.rpg wandering_hamster_*.txt
del wander.exe

REM Create custom package
support\pkzip distrib\custom.zip game.exe custom.exe hspeak.exe unlump.exe
support\pkzip distrib\custom.zip ohrrpgce.mas ohrrpgce.fnt ohrrpgce.new custom.txt whatsnew.txt LICENSE.txt
support\pkzip distrib\custom.zip sample.rpg npc_tag.rpg pstutor.rpg
support\pkzip distrib\custom.zip cleanup.bat plotscr.hsd scancode.hsi game.ico
support\pkzip distrib\custom.zip hamsterspeak.html plotscripttutor.html plotdictionary.html
support\pkzip distrib\custom.zip wanderp.hss wander.hsi
support\pkzip -P -r distrib\custom.zip import\*.*

REM Create ohrrpgce_play package
support\pkzip distrib\ohrrpgce_play.zip game.exe ohrrpgce.fnt game.txt game.ico

REM Create rpgfix package
support\pkzip distrib\rpgfix.zip rpgfix.exe lumpfix.exe fix.txt
