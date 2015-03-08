#!/bin/bash

echo "OHRRPGCE nightly build for Windows using Linux+Wine"
echo "---------------------------------------------------"

SCPHOST="james_paige@motherhamster.org"
SCPDEST="HamsterRepublic.com/ohrrpgce/nightly"
SCPDOCS="HamsterRepublic.com/ohrrpgce/docs"

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
  mustexist hspeak.exe
  mustexist relump.exe
  GFX="${1}"
  MUSIC="${2}"
  EXTRA="${3}"
  echo "Now uploading the OHR with ${GFX} graphics modules, and ${MUSIC} music module. (${EXTRA})"
  ZIPFILE="ohrrpgce-wip-${GFX}-${MUSIC}${EXTRA}.zip"
  if [ "${EXTRA}" = "~" ] ; then
    ZIPFILE=ohrrpgce-wip-${GFX}-${MUSIC}.zip
  fi

  rm -f distrib/"${ZIPFILE}"
  zip -q distrib/"${ZIPFILE}" game.exe custom.exe hspeak.exe
  zip -q distrib/"${ZIPFILE}" ohrrpgce.new
  zip -q -r distrib/"${ZIPFILE}" ohrhelp
  zip -q distrib/"${ZIPFILE}" support/madplay.exe
  zip -q distrib/"${ZIPFILE}" support/oggenc.exe
  zip -q distrib/"${ZIPFILE}" support/zip.exe
  cp relump.exe support/
  zip -q distrib/"${ZIPFILE}" support/relump.exe
  rm support/relump.exe
  rm -Rf texttemp
  mkdir texttemp
  cp whatsnew.txt *-binary.txt *-nightly.txt plotscr.hsd scancode.hsi svninfo.txt texttemp/
  unix2dos -q texttemp/*
  zip -q -j distrib/"${ZIPFILE}" texttemp/*
  rm -Rf texttemp

  mustexist distrib/"${ZIPFILE}"

  rm -Rf sanity
  mkdir sanity
  cd sanity
  unzip -qq ../distrib/"${ZIPFILE}"
  cd ..
  mustexist "sanity/game.exe"
  mustexist "sanity/custom.exe"
  rm -Rf sanity

  if [ -f "${4}" ] ; then
    zip -q distrib/"${ZIPFILE}" "${4}"
    shift
  fi

  scp distrib/"${ZIPFILE}" "${SCPHOST}":"${SCPDEST}"
}

#-----------------------------------------------------------------------
# turn of wine's debug noise
export WINEDEBUG=fixme-all

svn cleanup
svn update
svn info > svninfo.txt

./distrib-wine.sh nightly
OHRVERDATE=`svn info | grep "^Last Changed Date:" | cut -d ":" -f 2 | cut -d " " -f 2`
OHRVERCODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
SUFFIX="${OHRVERDATE}-${OHRVERCODE}"

mustexist distrib/ohrrpgce-win-installer-"${SUFFIX}".exe
scp -p distrib/ohrrpgce-win-installer-"${SUFFIX}".exe "${SCPHOST}":"${SCPDEST}"/ohrrpgce-wip-win-installer.exe

wine cmd /C "${SCONS}" hspeak relump.exe unlump.exe

rm -r game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=sdl debug=0
zip_and_upload directx sdl "~" gfx_directx.dll SDL.dll SDL_mixer.dll 

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+fb music=native debug=0
zip_and_upload directx native "~" gfx_directx.dll audiere.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+fb music=native2 debug=0
zip_and_upload directx native2 "~" gfx_directx.dll audiere.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=fb+directx+sdl music=sdl debug=0
zip_and_upload fb sdl "~" SDL.dll SDL_mixer.dll 

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=fb+directx music=native debug=0
zip_and_upload fb native "~" audiere.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=fb+directx music=native2 debug=0
zip_and_upload fb native2 "~" audiere.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=alleg+directx+fb+sdl music=sdl debug=0
zip_and_upload alleg sdl "~" alleg40.dll SDL.dll SDL_mixer.dll 

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=alleg+directx+fb music=native debug=0
zip_and_upload alleg native "~" alleg40.dll audiere.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=alleg+directx+fb music=native2 debug=0
zip_and_upload alleg native2 "~" alleg40.dll audiere.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=sdl+directx+fb music=sdl debug=0
zip_and_upload sdl sdl "~" SDL.dll SDL_mixer.dll 

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=sdl+directx+fb music=native debug=0
zip_and_upload sdl native "~" audiere.dll SDL.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=sdl+directx+fb music=native2 debug=0
zip_and_upload sdl native2 "~" audiere.dll SDL.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=silence debug=0
zip_and_upload directx silence "~" SDL.dll gfx_directx.dll

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=sdl debug=2
zip_and_upload directx sdl -debug SDL.dll SDL_mixer.dll gfx_directx.dll misc/gdbcmds1.txt misc/gdbcmds2.txt gdbgame.bat gdbcustom.bat

rm -f game*.exe custom*.exe
wine cmd /C "${SCONS}" gfx=directx+sdl+fb music=sdl debug=0 scriptprofile=1
zip_and_upload directx sdl -scriptprofile SDL.dll SDL_mixer.dll gfx_directx.dll

# Note that this is duplicated in distrib-nightly.sh
echo "uploading plotscripting docs"
scp docs/plotdict.xml "${SCPHOST}":"${SCPDOCS}"
scp docs/htmlplot.xsl "${SCPHOST}":"${SCPDOCS}"
docs/update-html.sh
scp docs/plotdictionary.html "${SCPHOST}":"${SCPDOCS}"

rm -f distrib/ohrrpgce-util.zip
rm -f unlump.exe relump.exe
wine cmd /C "${SCONS}" unlump.exe relump.exe
mustexist unlump.exe
mustexist relump.exe
zip distrib/ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt svninfo.txt
scp distrib/ohrrpgce-util.zip "${SCPHOST}":"${SCPDEST}"

rm -f distrib/hspeak-win-nightly.zip
rm -f hspeak.exe
wine cmd /C "${SCONS}" hspeak
mustexist hspeak.exe
zip distrib/hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e LICENSE.txt plotscr.hsd
scp distrib/hspeak-win-nightly.zip "${SCPHOST}":"${SCPDEST}"

rm -f distrib/bam2mid.zip
rm -f bam2mid.exe
wine cmd /C "${SCONS}" bam2mid.exe
mustexist bam2mid.exe
zip distrib/bam2mid.zip bam2mid.exe bam2mid.txt bam2mid.bas banks.bi LICENSE.txt make-bam2mid.bat make-bam2mid.sh svninfo.txt
scp distrib/bam2mid.zip "${SCPHOST}":"${SCPDEST}"

rm -f distrib/madplay+oggenc.zip
zip distrib/madplay+oggenc.zip support/madplay.exe support/oggenc.exe support/LICENSE-*.txt LICENSE.txt
scp distrib/madplay+oggenc.zip "${SCPHOST}":"${SCPDEST}"

scp svninfo.txt "${SCPHOST}":"${SCPDEST}"
