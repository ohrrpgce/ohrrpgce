REM *WARNING* Do not schedule this batch file to be automatically
REM run from the same copy of the sources that it updates. That
REM would be equivalent to allowing any developer with write access
REM to the repository full control of your computer. Instead schedule
REM this script to be run from a manually updated copy, and pay
REM attention to changes to it.

cd c:\nightly\ohrrpgce
svn update
del game*.exe
del custom*.exe

call makegame-gfx-music fb native
call makeedit-gfx-music fb native
call nightly-gfx-music fb native

call makegame-gfx-music fb sdl
call makeedit-gfx-music fb sdl
call nightly-gfx-music fb sdl

call makegame-gfx-music fb2 native
call makeedit-gfx-music fb2 native
call nightly-gfx-music fb2 native

call makegame-gfx-music fb2 sdl
call makeedit-gfx-music fb2 sdl
call nightly-gfx-music fb2 sdl

call makegame-gfx-music alleg native
call makeedit-gfx-music alleg native
call nightly-gfx-music alleg native

call makegame-gfx-music alleg sdl
call makeedit-gfx-music alleg sdl
call nightly-gfx-music alleg sdl

call makegame-gfx-music alleg allegro
call makeedit-gfx-music alleg allegro
call nightly-gfx-music alleg allegro

Echo upload plotdict.xml
pscp -i C:\progra~1\putty\id_rsa.ppk docs\plotdict.xml spam@brionne.cyberverse.com:web/html/ohrrpgce/docs/

del distrib\hspeak-dos-nightly.zip
call bind -clear_routines hspeak.ex
support\zip distrib\hspeak-dos-nightly.zip hspeak.exe hspeak.ex LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\hspeak-dos-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/nightly/

copy hspeak.ex hspeak.exw
del distrib\hspeak-win-nightly.zip
call bindw -clear_routines hspeak.exw
support\zip distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\hspeak-win-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/nightly/
del hspeak.exw

