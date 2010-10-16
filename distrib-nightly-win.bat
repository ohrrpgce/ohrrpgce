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
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-win-installer-%OHRVERDATE%-%OHRVERCODE%.exe james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/ohrrpgce-wip-win-installer.exe

REM This build is a copy of the default
del game*.exe
del custom*.exe
call makeboth.bat directx sdl fb ~ sdl
call nightly-gfx-music directx sdl ~ gfx_directx.dll SDL.dll SDL_mixer.dll 

del game*.exe
del custom*.exe
call makeboth.bat directx fb ~ native
call nightly-gfx-music directx native ~ gfx_directx.dll audiere.dll

del game*.exe
del custom*.exe
call makeboth.bat directx fb ~ native2
call nightly-gfx-music directx native2 ~ gfx_directx.dll audiere.dll

del game*.exe
del custom*.exe
call makeboth.bat fb directx sdl ~ sdl
call nightly-gfx-music fb sdl ~ SDL.dll SDL_mixer.dll 

del game*.exe
del custom*.exe
call makeboth.bat fb directx ~ native
call nightly-gfx-music fb native ~ audiere.dll

del game*.exe
del custom*.exe
call makeboth.bat fb directx ~ native2
call nightly-gfx-music fb native2 ~ audiere.dll

del game*.exe
del custom*.exe
call makeboth.bat alleg directx fb sdl ~ sdl
call nightly-gfx-music alleg sdl ~ alleg40.dll SDL.dll SDL_mixer.dll 

del game*.exe
del custom*.exe
call makeboth.bat alleg directx fb ~ native
call nightly-gfx-music alleg native ~ alleg40.dll audiere.dll

del game*.exe
del custom*.exe
call makeboth.bat alleg directx fb ~ native2
call nightly-gfx-music alleg native2 ~ alleg40.dll audiere.dll

del game*.exe
del custom*.exe
call makeboth.bat sdl directx fb ~ sdl
call nightly-gfx-music sdl sdl ~ SDL.dll SDL_mixer.dll 

del game*.exe
del custom*.exe
call makeboth.bat sdl directx fb ~ native
call nightly-gfx-music sdl native ~ audiere.dll SDL.dll

del game*.exe
del custom*.exe
call makeboth.bat sdl directx fb ~ native2
call nightly-gfx-music sdl native2 ~ audiere.dll SDL.dll

del game*.exe
del custom*.exe
call makeboth.bas directx sdl fb ~ silence
call nightly-gfx-music directx silence ~ SDL.dll gfx_directx.dll

del game*.exe
del custom*.exe
call makeboth.bat directx sdl fb ~ sdl -g
call nightly-gfx-music directx sdl -debug SDL.dll SDL_mixer.dll gfx_directx.dll

del game*.exe
del custom*.exe
call makeboth.bat directx sdl fb ~ sdl -g -exx -s console
call nightly-gfx-music directx sdl -debug-exx SDL.dll SDL_mixer.dll gfx_directx.dll

del game*.exe
del custom*.exe
call makeboth.bat directx sdl fb ~ sdl -g -d SCRIPTPROFILE
call nightly-gfx-music directx sdl -scriptprofile SDL.dll SDL_mixer.dll gfx_directx.dll

Echo upload plotdict.xml
pscp -i C:\progra~1\putty\id_rsa.ppk docs\plotdict.xml james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/docs/

call makeutil.bat
del distrib\ohrrpgce-util.zip
IF NOT EXIST unlump.exe GOTO NOUTIL
IF NOT EXIST relump.exe GOTO NOUTIL
support\zip distrib\ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt svninfo.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-util.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOUTIL

del distrib\hspeak-win-nightly.zip
call makehspeak.bat
IF NOT EXIST hspeak.exe GOTO NOHSPEAK
support\zip distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e LICENSE.txt plotscr.hsd
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\hspeak-win-nightly.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOHSPEAK

del distrib\bam2mid.zip
call make-bam2mid.bat
IF NOT EXIST bam2mid.exe GOTO NOBAM2MID
support\zip distrib\bam2mid.zip bam2mid.exe bam2mid.txt bam2mid.bas banks.bi LICENSE.txt make-bam2mid.bat make-bam2mid.sh svninfo.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\bam2mid.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOBAM2MID

del distrib\madplay+oggenc.zip
support\zip distrib\madplay+oggenc.zip support\madplay.exe support\oggenc.exe support\LICENSE-*.txt LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\madplay+oggenc.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
