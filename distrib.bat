cd %OHRRPGCE%
copy whatsnew.txt distrib
copy howto.txt distrib
copy custom.txt distrib
copy wh_demo.txt distrib
copy plotscripttutor.html distrib
copy plotdictionary.html distrib
copy hamsterspeak.html distrib
copy sample\*.rpg .
pkzip distrib\wh_demo.zip game.exe wander.rpg ohrrpgce.mas ohrrpgce.fnt wh_demo.txt
pkzip distrib\custom.zip game.exe custom.exe hspeak.exe rpgfix.exe
pkzip distrib\custom.zip ohrrpgce.mas ohrrpgce.fnt ohrrpgce.new custom.txt whatsnew.txt howto.txt
pkzip distrib\custom.zip sample.rpg npc_tag.rpg pstutor.rpg
pkzip distrib\custom.zip cleanup.bat plotscr.hsd game.ico
pkzip distrib\custom.zip hamsterspeak.html plotscripttutor.html plotdictionary.html
pkzip distrib\custom.zip wanderp.hss wander.hsi
pkzip -P -r distrib\custom.zip import\*.*
