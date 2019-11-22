REM *WARNING* Scheduling this batch file to be automatically
REM run is equivalent to allowing any developer with write access
REM to the repository full control of your build computer. Thank
REM goodness James trusts the other devs ;)

CALL distrib-win-setup.bat || exit /b 1

cd c:\nightly\ohrrpgce
svn cleanup
svn update > nightly-temp.txt
IF errorlevel 1 (
    TYPE nightly-temp.txt
    exit /b 1
)
TYPE nightly-temp.txt

REM "At revision" means no change, vs "Updated to revision"
TYPE nightly-temp.txt | FIND "At revision" > NUL && (
  echo No changes, no need to update nightly.
  del nightly-temp.txt
  exit /b 0
)
del nightly-temp.txt

svn info > svninfo.txt

REM Build all utilities once
CALL scons hspeak relump unlump %SCONS_ARGS%

REM This is the default build (default download is symlinked to it on the server)
support\rm -f game.exe custom.exe
call scons music=sdl %SCONS_ARGS%
call distrib-nightly-win-packnupload music_sdl gfx_directx.dll SDL.dll SDL_mixer.dll

ECHO Packaging ohrrpgce-win-installer-wip.exe ...
REM Create the installer from the executables we just built: the installer and .zips for default build configs
REM must contain the same executables, to share .pdb files
support\rm -f distrib\ohrrpgce-win-installer-wip.exe
echo InfoBeforeFile=IMPORTANT-nightly.txt > iextratxt.txt
"%ISCC%" /Q /Odistrib /Fohrrpgce-win-installer-wip ohrrpgce.iss
del iextratxt.txt
IF EXIST distrib\ohrrpgce-win-installer-wip.exe (
    pscp -q distrib\ohrrpgce-win-installer-wip.exe %SCPHOST%:%SCPDEST%
)

support\rm -f game.exe custom.exe
call scons music=native %SCONS_ARGS%
call distrib-nightly-win-packnupload music_native gfx_directx.dll SDL.dll audiere.dll

support\rm -f game.exe custom.exe
call scons music=native2 %SCONS_ARGS%
call distrib-nightly-win-packnupload music_native2 gfx_directx.dll SDL.dll audiere.dll

support\rm -f game.exe custom.exe
call scons music=silence %SCONS_ARGS%
call distrib-nightly-win-packnupload music_silence gfx_directx.dll SDL.dll

REM support\rm -f game.exe custom.exe
REM call scons gfx=alleg+directx+fb+sdl music=sdl %SCONS_ARGS%
REM call distrib-nightly-win-packnupload gfx_alleg-music_sdl alleg40.dll SDL.dll SDL_mixer.dll

support\rm -f game.exe custom.exe
call scons gfx=sdl2+directx+fb music=sdl2 %SCONS_ARGS%
call distrib-nightly-win-packnupload sdl2 gfx_directx.dll SDL2.dll SDL2_mixer.dll

support\rm -f game.exe custom.exe
call scons music=sdl debug=2 pdb=1
call distrib-nightly-win-packnupload music_sdl-debug gfx_directx.dll SDL.dll SDL_mixer.dll misc\gdbcmds1.txt misc\gdbcmds2.txt gdbgame.bat gdbcustom.bat

REM Note that this is duplicated in distrib-nightly-linux.sh
Echo upload plotdict.xml
pscp -q docs\*.png %SCPHOST%:%SCPDOCS%
pscp -q docs\plotdict.xml %SCPHOST%:%SCPDOCS%
pscp -q docs\htmlplot.xsl %SCPHOST%:%SCPDOCS%

support\rm -f distrib\ohrrpgce-util.zip
IF NOT EXIST unlump.exe GOTO NOUTIL
IF NOT EXIST relump.exe GOTO NOUTIL
support\zip distrib\ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt svninfo.txt
pscp -q distrib\ohrrpgce-util.zip %SCPHOST%:%SCPDEST%
:NOUTIL

support\rm -f distrib\hspeak-win-nightly.zip
IF NOT EXIST hspeak.exe GOTO NOHSPEAK
support\zip distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e euphoria\*.e euphoria\License.txt LICENSE.txt plotscr.hsd scancode.hsi
pscp -q distrib\hspeak-win-nightly.zip %SCPHOST%:%SCPDEST%
:NOHSPEAK

support\rm -f distrib\bam2mid.zip bam2mid.exe
call scons bam2mid.exe
IF NOT EXIST bam2mid.exe GOTO NOBAM2MID
support\zip distrib\bam2mid.zip bam2mid.exe bam2mid.txt LICENSE.txt svninfo.txt
pscp -q distrib\bam2mid.zip %SCPHOST%:%SCPDEST%
:NOBAM2MID

support\rm -f distrib\madplay+oggenc.zip
support\zip distrib\madplay+oggenc.zip support\madplay.exe support\oggenc.exe support\LICENSE-madplay.txt support\LICENSE-oggenc.txt
pscp -q distrib\madplay+oggenc.zip %SCPHOST%:%SCPDEST%

pscp -q svninfo.txt %SCPHOST%:%SCPDEST%
