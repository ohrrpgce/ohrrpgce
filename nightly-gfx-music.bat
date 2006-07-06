@echo off
IF NOT EXIST gfx_%1.bas GOTO failed
IF NOT EXIST music_%2.bas GOTO failed
echo Now uploading the OHR with %1 graphics module, and %2 music module

del distrib\ohrrpgce-wip-%1-%2.zip
support\pkzip distrib\ohrrpgce-wip-%1-%2.zip game.exe custom.exe > NUL
support\pkzip distrib\ohrrpgce-wip-%1-%2.zip whatsnew.txt *-binary.txt *-nightly.txt plotscr.hsd > NUL

IF NOT EXIST distrib\ohrrpgce-wip-%1-%2.zip GOTO failed

mkdir sanity
cd sanity
..\support\pkunzip ..\distrib\ohrrpgce-wip-%1-%2.zip > NUL
cd ..
IF NOT EXIST sanity\game.exe GOTO sanityfailed
IF NOT EXIST sanity\custom.exe GOTO sanityfailed
del sanity\*.exe
del sanity\*.txt
del sanity\*.hsd
rmdir sanity

IF NOT %1==alleg GOTO skipgfxalleg
support\pkzip distrib\ohrrpgce-wip-%1-%2.zip alleg40.dll > NUL
:skipgfxalleg

IF NOT %2==allegro GOTO skipmusalleg
support\pkzip distrib\ohrrpgce-wip-%1-%2.zip alleg40.dll > NUL
:skipmusalleg

IF NOT %2==sdl GOTO skipmussdl
support\pkzip distrib\ohrrpgce-wip-%1-%2.zip SDL.dll SDL_mixer.dll > NUL
:skipmussdl

pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-wip-%1-%2.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/nightly/
GOTO finished

:sanityfailed
del sanity\*.exe
del sanity\*.dll
del sanity\*.txt
del sanity\*.hsd
rmdir sanity

:failed

:finished
