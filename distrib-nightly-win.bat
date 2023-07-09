REM *WARNING* Scheduling this batch file to be automatically
REM run is equivalent to allowing any developer with write access
REM to the repository full control of your build computer. Thank
REM goodness James trusts the other devs ;)

CALL distrib-win-setup.bat || exit /b 1
set SCONS_CACHE_SIZE=200

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

REM -----------------------------------------------------------------------

REM Build all utilities once
REM Would compile with lto=1 to reduce unlump/relump size, but that causes mingw-w64 gcc 8.1.0 to crash
CALL scons hspeak relump unlump win95=1 sse2=0 %SCONS_ARGS% || exit /b 1

REM This is the build for obsolete Windows machines (symlinked as ohrrpgce-win-win95-wip.zip)
call scons gfx=directx+sdl+fb music=sdl win95=1 sse2=0 buildname=win95 %SCONS_ARGS% && (
  call distrib-nightly-win-packnupload win95

  ECHO     Packaging win95 game player ...
  python ohrpackage.py win player distrib\ohrrpgce-player-win-wip-win95.zip && (
    pscp -q distrib\ohrrpgce-player-win-wip-win95.zip %SCPHOST%:%SCPDEST%
  )
)

REM This is the default build (default download ohrrpgce-win-default.zip is symlinked to it on the server)
call scons gfx=sdl2+directx+fb music=sdl2 buildname=sdl2 %SCONS_ARGS% && (
  call distrib-nightly-win-packnupload sdl2

  ECHO     Packaging ohrrpgce-win-installer-wip.exe ...
  REM Create the installer from the executables we just built: the installer and .zips for default build configs
  REM must contain the same executables, to share .pdb files
  python ohrpackage.py win full+vikings distrib\ohrrpgce-win-installer-wip.exe --iscc "%ISCC%" && (
    pscp -q distrib\ohrrpgce-win-installer-wip.exe %SCPHOST%:%SCPDEST%
  )

  ECHO     Packaging sdl2 game player ...
  python ohrpackage.py win player distrib\ohrrpgce-player-win-wip-sdl2.zip && (
    pscp -q distrib\ohrrpgce-player-win-wip-sdl2.zip %SCPHOST%:%SCPDEST%
  )
)

call scons music=native buildname=music_native %SCONS_ARGS% && (
  call distrib-nightly-win-packnupload music_native
)

call scons music=native2 buildname=music_native2 %SCONS_ARGS% && (
  call distrib-nightly-win-packnupload music_native2
)

call scons music=silence buildname=music_silence %SCONS_ARGS% && (
  call distrib-nightly-win-packnupload music_silence
)

call scons debug=2 pdb=1 buildname=sdl2-debug && (
  call distrib-nightly-win-packnupload sdl2-debug misc\gdbcmds1.txt gdbgame.bat gdbcustom.bat
)

ECHO     Packaging other utilities

REM Note that this is duplicated in distrib-nightly-linux.sh
pscp -q docs\*.png %SCPHOST%:%SCPDOCS%
pscp -q docs\plotdict.xml %SCPHOST%:%SCPDOCS%
pscp -q docs\htmlplot.xsl %SCPHOST%:%SCPDOCS%

support\rm -f distrib\ohrrpgce-util.zip
IF NOT EXIST unlump.exe GOTO NOUTIL
IF NOT EXIST relump.exe GOTO NOUTIL
support\zip -q distrib\ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt svninfo.txt
pscp -q distrib\ohrrpgce-util.zip %SCPHOST%:%SCPDEST%
:NOUTIL

support\rm -f distrib\hspeak-win-nightly.zip
IF NOT EXIST hspeak.exe GOTO NOHSPEAK
support\zip -q distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e euphoria\*.e euphoria\License.txt LICENSE.txt plotscr.hsd scancode.hsi
pscp -q distrib\hspeak-win-nightly.zip %SCPHOST%:%SCPDEST%
:NOHSPEAK

support\rm -f distrib\bam2mid.zip bam2mid.exe
call scons bam2mid.exe
IF NOT EXIST bam2mid.exe GOTO NOBAM2MID
support\zip -q distrib\bam2mid.zip bam2mid.exe bam2mid.txt LICENSE.txt svninfo.txt
pscp -q distrib\bam2mid.zip %SCPHOST%:%SCPDEST%
:NOBAM2MID

support\rm -f distrib\madplay+oggenc.zip
support\zip -q distrib\madplay+oggenc.zip support\madplay.exe support\oggenc.exe support\LICENSE-madplay.txt support\LICENSE-oggenc.txt
pscp -q distrib\madplay+oggenc.zip %SCPHOST%:%SCPDEST%

REM For some weird reason, the following upload only works once every few months
pscp -q svninfo.txt %SCPHOST%:%SCPDEST%
