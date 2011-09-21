#!/bin/bash

echo "OHRRPGCE nightly build for Windows using Linux+Wine"
echo "---------------------------------------------------"

SCPHOST="james_paige@motherhamster.org"
SCPDEST="HamsterRepublic.com/ohrrpgce/nightly"

SCONS="C:\Python27\Scripts\scons.bat"

#-----------------------------------------------------------------------

function mustexist {
  if [ ! -f "${1}" -a ! -d "${1}" ] ; then
    echo "ERROR: ${1} does not exist!"
    exit 1
  fi
}

function zip_and_upload {
  mustexist game.exe
  mustexist custom.exe
  GFX="${1}"
  MUSIC="${2}"
  EXTRA="${3}"
  echo "Now uploading the OHR with ${GFX} graphics modules, and ${MUSIC} music module. (${EXTRA})"
  ZIPFILE="ohrrpgce-wip-${GFX}-${MUSIC}${EXTRA}.zip"
  if [ "${EXTRA}" = "~" ] ; then
    ZIPFILE=ohrrpgce-wip-${GFX}-${MUSIC}.zip
  fi

  rm -f distrib/"${ZIPFILE}"
  zip -q distrib/"${ZIPFILE}" game.exe custom.exe
  zip -q distrib/"${ZIPFILE}" ohrrpgce.new
  zip -q distrib/"${ZIPFILE}" whatsnew.txt *-binary.txt *-nightly.txt plotscr.hsd svninfo.txt
  zip -q -r distrib/"${ZIPFILE}" ohrhelp

IF NOT EXIST distrib\%ZIPFILE% GOTO failed

mkdir sanity
cd sanity
..\support\unzip -qq ..\distrib\%ZIPFILE%
cd ..
IF NOT EXIST sanity\game.exe GOTO sanityfailed
IF NOT EXIST sanity\custom.exe GOTO sanityfailed
support\rm -r sanity

:addextrafiles

IF NOT EXIST "%4" GOTO extrafilesdone
support\zip -q distrib\%ZIPFILE% %4
shift
goto addextrafiles
:extrafilesdone

pscp -i C:\progra~1\putty\id_rsa.ppk distrib\%ZIPFILE% james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
GOTO finished

:sanityfailed
del sanity\*.exe
del sanity\*.dll
del sanity\*.txt
del sanity\*.hsd
rmdir sanity

:failed

:finished
}

#-----------------------------------------------------------------------
# turn of wine's debug noise
export WINEDEBUG=fixme-all

svn cleanup
svn update
svn info > svninfo.txt

./distrib-wine.sh nightly
OHRVERDATE=`svn info | grep "^Last Changed Date:" | cut -d ":" -f 2 | cut -d " " -f 2`
OHRVERCODE=`cat codename.txt`
SUFFIX="${OHRVERDATE}-${OHRVERCODE}"

mustexist distrib/ohrrpgce-win-installer-"${SUFFIX}".exe
scp -p distrib/ohrrpgce-win-installer-"${SUFFIX}".exe "${SCPHOST}":"${SCPDEST}"/ohrrpgce-wip-win-installer.exe

rm -r game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=sdl debug=0
call nightly-gfx-music directx sdl ~ gfx_directx.dll SDL.dll SDL_mixer.dll 

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+fb music=native debug=0
call nightly-gfx-music directx native ~ gfx_directx.dll audiere.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+fb music=native2 debug=0
call nightly-gfx-music directx native2 ~ gfx_directx.dll audiere.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=fb+directx+sdl music=sdl debug=0
call nightly-gfx-music fb sdl ~ SDL.dll SDL_mixer.dll 

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=fb+directx music=native debug=0
call nightly-gfx-music fb native ~ audiere.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=fb+directx music=native2 debug=0
call nightly-gfx-music fb native2 ~ audiere.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=alleg+directx+fb+sdl music=sdl debug=0
call nightly-gfx-music alleg sdl ~ alleg40.dll SDL.dll SDL_mixer.dll 

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=alleg+directx+fb music=native debug=0
call nightly-gfx-music alleg native ~ alleg40.dll audiere.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=alleg+directx+fb music=native2 debug=0
call nightly-gfx-music alleg native2 ~ alleg40.dll audiere.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=sdl+directx+fb music=sdl debug=0
call nightly-gfx-music sdl sdl ~ SDL.dll SDL_mixer.dll 

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=sdl+directx+fb music=native debug=0
call nightly-gfx-music sdl native ~ audiere.dll SDL.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=sdl+directx+fb music=native2 debug=0
call nightly-gfx-music sdl native2 ~ audiere.dll SDL.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=silence debug=0
call nightly-gfx-music directx silence ~ SDL.dll gfx_directx.dll

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=sdl debug=1
call nightly-gfx-music directx sdl -debug SDL.dll SDL_mixer.dll gfx_directx.dll misc\gdbcmds1.txt misc\gdbcmds2.txt gdbgame.bat gdbcustom.bat

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=sdl debug=1 valgrind=1
call nightly-gfx-music directx sdl -debug-valgrind SDL.dll SDL_mixer.dll gfx_directx.dll misc\gdbcmds1.txt misc\gdbcmds2.txt gdbgame.bat gdbcustom.bat

del game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=sdl debug=0 scriptprofile=1
call nightly-gfx-music directx sdl -scriptprofile SDL.dll SDL_mixer.dll gfx_directx.dll

Echo upload plotdict.xml
pscp -i C:\progra~1\putty\id_rsa.ppk docs\plotdict.xml james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/docs/

del unlump.exe relump.exe
wine cmd /C "${SCONS}" unlump.exe relump.exe
del distrib\ohrrpgce-util.zip
IF NOT EXIST unlump.exe GOTO NOUTIL
IF NOT EXIST relump.exe GOTO NOUTIL
support\zip distrib\ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt svninfo.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-util.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOUTIL

del distrib\hspeak-win-nightly.zip
del hspeak.exe
wine cmd /C "${SCONS}" hspeak
IF NOT EXIST hspeak.exe GOTO NOHSPEAK
support\zip distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e LICENSE.txt plotscr.hsd
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\hspeak-win-nightly.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOHSPEAK

del distrib\bam2mid.zip
del bam2mid.exe
wine cmd /C "${SCONS}" bam2mid.exe
IF NOT EXIST bam2mid.exe GOTO NOBAM2MID
support\zip distrib\bam2mid.zip bam2mid.exe bam2mid.txt bam2mid.bas banks.bi LICENSE.txt make-bam2mid.bat make-bam2mid.sh svninfo.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\bam2mid.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOBAM2MID

del distrib\madplay+oggenc.zip
support\zip distrib\madplay+oggenc.zip support\madplay.exe support\oggenc.exe support\LICENSE-*.txt LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\madplay+oggenc.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/

pscp -i C:\progra~1\putty\id_rsa.ppk svninfo.txt james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
