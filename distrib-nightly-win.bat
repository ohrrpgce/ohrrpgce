REM *WARNING* Scheduling this batch file to be automatically
REM run is equivalent to allowing any developer with write access
REM to the repository full control of your build computer. Thank
REM goodness James trusts the other devs ;)

cd c:\nightly\ohrrpgce
svn cleanup
svn update
svn info > svninfo.txt

CALL distrib.bat nightly
CALL distver.bat
pscp -q distrib\ohrrpgce-win-installer-%OHRVERDATE%-%OHRVERCODE%.exe james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/ohrrpgce-wip-win-installer.exe

CALL scons hspeak relump.exe unlump.exe

del game*.exe custom*.exe
call scons gfx=directx+sdl+fb music=sdl debug=0
call nightly-gfx-music directx sdl ~ gfx_directx.dll SDL.dll SDL_mixer.dll 

del game*.exe custom*.exe
call scons gfx=directx+fb music=native debug=0
call nightly-gfx-music directx native ~ gfx_directx.dll audiere.dll

del game*.exe custom*.exe
call scons gfx=directx+fb music=native2 debug=0
call nightly-gfx-music directx native2 ~ gfx_directx.dll audiere.dll

del game*.exe custom*.exe
call scons gfx=fb+directx+sdl music=sdl debug=0
call nightly-gfx-music fb sdl ~ SDL.dll SDL_mixer.dll 

del game*.exe custom*.exe
call scons gfx=fb+directx music=native debug=0
call nightly-gfx-music fb native ~ audiere.dll

del game*.exe custom*.exe
call scons gfx=fb+directx music=native2 debug=0
call nightly-gfx-music fb native2 ~ audiere.dll

del game*.exe custom*.exe
call scons gfx=alleg+directx+fb+sdl music=sdl debug=0
call nightly-gfx-music alleg sdl ~ alleg40.dll SDL.dll SDL_mixer.dll 

del game*.exe custom*.exe
call scons gfx=alleg+directx+fb music=native debug=0
call nightly-gfx-music alleg native ~ alleg40.dll audiere.dll

del game*.exe custom*.exe
call scons gfx=alleg+directx+fb music=native2 debug=0
call nightly-gfx-music alleg native2 ~ alleg40.dll audiere.dll

del game*.exe custom*.exe
call scons gfx=sdl+directx+fb music=sdl debug=0
call nightly-gfx-music sdl sdl ~ SDL.dll SDL_mixer.dll 

del game*.exe custom*.exe
call scons gfx=sdl+directx+fb music=native debug=0
call nightly-gfx-music sdl native ~ audiere.dll SDL.dll

del game*.exe custom*.exe
call scons gfx=sdl+directx+fb music=native2 debug=0
call nightly-gfx-music sdl native2 ~ audiere.dll SDL.dll

del game*.exe custom*.exe
call scons gfx=directx+sdl+fb music=silence debug=0
call nightly-gfx-music directx silence ~ SDL.dll gfx_directx.dll

del game*.exe custom*.exe
call scons gfx=directx+sdl+fb music=sdl debug=2
call nightly-gfx-music directx sdl -debug SDL.dll SDL_mixer.dll gfx_directx.dll misc\gdbcmds1.txt misc\gdbcmds2.txt gdbgame.bat gdbcustom.bat

del game*.exe custom*.exe
call scons gfx=directx+sdl+fb music=sdl debug=2 valgrind=1
call nightly-gfx-music directx sdl -debug-valgrind SDL.dll SDL_mixer.dll gfx_directx.dll misc\gdbcmds1.txt misc\gdbcmds2.txt gdbgame.bat gdbcustom.bat

del game*.exe custom*.exe
call scons gfx=directx+sdl+fb music=sdl debug=0 scriptprofile=1
call nightly-gfx-music directx sdl -scriptprofile SDL.dll SDL_mixer.dll gfx_directx.dll

Echo upload plotdict.xml
pscp -q docs\plotdict.xml james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/docs/

del unlump.exe relump.exe
call scons unlump.exe relump.exe
del distrib\ohrrpgce-util.zip
IF NOT EXIST unlump.exe GOTO NOUTIL
IF NOT EXIST relump.exe GOTO NOUTIL
support\zip distrib\ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt svninfo.txt
pscp -q distrib\ohrrpgce-util.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOUTIL

del distrib\hspeak-win-nightly.zip
del hspeak.exe
call scons hspeak
IF NOT EXIST hspeak.exe GOTO NOHSPEAK
support\zip distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e LICENSE.txt plotscr.hsd
pscp -q distrib\hspeak-win-nightly.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOHSPEAK

del distrib\bam2mid.zip
del bam2mid.exe
call scons bam2mid.exe
IF NOT EXIST bam2mid.exe GOTO NOBAM2MID
support\zip distrib\bam2mid.zip bam2mid.exe bam2mid.txt bam2mid.bas banks.bi LICENSE.txt make-bam2mid.bat make-bam2mid.sh svninfo.txt
pscp -q distrib\bam2mid.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOBAM2MID

del distrib\madplay+oggenc.zip
support\zip distrib\madplay+oggenc.zip support\madplay.exe support\oggenc.exe support\LICENSE-*.txt LICENSE.txt
pscp -q distrib\madplay+oggenc.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/

pscp -q svninfo.txt james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
