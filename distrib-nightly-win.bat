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
move game.exe game-fb-native.exe
move custom.exe custom-fb-native.exe

call makegame-gfx-music fb sdl
call makeedit-gfx-music fb sdl
move game.exe game-fb-sdl.exe
move custom.exe custom-fb-sdl.exe

call makegame-gfx-music fb2 native
call makeedit-gfx-music fb2 native
move game.exe game-fb2-native.exe
move custom.exe custom-fb2-native.exe

call makegame-gfx-music fb2 sdl
call makeedit-gfx-music fb2 sdl
move game.exe game-fb2-sdl.exe
move custom.exe custom-fb2-sdl.exe

call makegame-gfx-music alleg native
call makeedit-gfx-music alleg native
move game.exe game-alleg-native.exe
move custom.exe custom-alleg-native.exe

call makegame-gfx-music alleg sdl
call makeedit-gfx-music alleg sdl
move game.exe game-alleg-sdl.exe
move custom.exe custom-alleg-sdl.exe

call makegame-gfx-music alleg allegro
call makeedit-gfx-music alleg allegro
move game.exe game-alleg-allegro.exe
move custom.exe custom-alleg-allegro.exe

call env-set.bat
SET OHRRPGCE=C:\NIGHTLY\OHRRPGCE
call compile.bat

del distrib\ohrrpgce-binary-win-nightly.zip
support\pkzip distrib\ohrrpgce-binary-win-nightly.zip game*.exe custom*.exe
support\pkzip distrib\ohrrpgce-binary-win-nightly.zip whatsnew.txt *-binary.txt *-nightly.txt SDL.dll SDL_mixer.dll alleg40.dll
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-binary-win-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/archive/

del distrib\hspeak-dos-nightly.zip
call bind -clear_routines hspeak.ex
support\pkzip distrib\hspeak-dos-nightly.zip hspeak.exe hspeak.ex LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\hspeak-dos-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/archive/

copy hspeak.ex hspeak.exw
del distrib\hspeak-win-nightly.zip
call bindw -clear_routines hspeak.exw
support\pkzip distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\hspeak-win-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/archive/
del hspeak.exw

