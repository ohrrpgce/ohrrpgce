#!/bin/bash

echo "OHRRPGCE nightly build for Windows using Linux+Wine"
echo "---------------------------------------------------"

SCPHOST="james_paige@motherhamster.org"
SCPDEST="HamsterRepublic.com/ohrrpgce/nightly"
SCPDOCS="HamsterRepublic.com/ohrrpgce/nightly/docs"
SCPSYMBOLS="HamsterRepublic.com/ohrrpgce/symbols-archive"

ISCC="C:\Program Files\Inno Setup 5\iscc.exe"
SCONS="C:\Python27\Scripts\scons.bat"

SCONS_ARGS="release=1 pdb=1"

# Using wine
BUILD="wine cmd /C ${SCONS}"

# Uncomment to cross-compile
#export PATH=~/src/mxe/usr/bin:$PATH
#BUILD="scons target=i686-w64-mingw32.shared"
#DONT_BUILD_HSPEAK=yes  #TODO: scons doesn't support cross-compiling hspeak yet

OHRVERDATE=`svn info | grep "^Last Changed Date:" | cut -d ":" -f 2 | cut -d " " -f 2`
SVNREV=`svn info | grep "^Revision:" | cut -d " " -f 2`

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
  BUILDNAME="${1}"
  ZIPFILE="ohrrpgce-win-${BUILDNAME}-wip.zip"
  SYMBFILE="ohrrpgce-symbols-win-${BUILDNAME}-r${SVNREV}-${OHRVERDATE}-wip.7z"
  echo "Now creating ${ZIPFILE}"

  rm -f distrib/"${ZIPFILE}"
  zip -q distrib/"${ZIPFILE}" game.exe custom.exe hspeak.exe
  zip -q -r distrib/"${ZIPFILE}" data
  zip -q -r distrib/"${ZIPFILE}" ohrhelp
  zip -q distrib/"${ZIPFILE}" docs/plotdictionary.html docs/*.png docs/more-docs.txt
  zip -q distrib/"${ZIPFILE}" support/madplay.exe support/LICENSE-madplay.txt
  zip -q distrib/"${ZIPFILE}" support/oggenc.exe support/LICENSE-oggenc.txt
  zip -q distrib/"${ZIPFILE}" support/zip.exe
  zip -q distrib/"${ZIPFILE}" support/wget.exe
  zip -q distrib/"${ZIPFILE}" support/CrashRpt*.dll support/CrashSender*.exe support/crashrpt_lang.ini
  zip -q distrib/"${ZIPFILE}" support/LICENSE-crashrpt.txt
  cp relump.exe support/
  zip -q distrib/"${ZIPFILE}" support/relump.exe
  rm support/relump.exe
  # unlump.exe is excluded
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

  while [ -f "${2}" ] ; do
    zip -q distrib/"${ZIPFILE}" "${2}"
    shift
  done

  echo "Now creating ${SYMBFILE}"
  rm -f distrib/"${SYMBFILE}"
  if [ -f "win32/game.pdb" -a -f "win32/custom.pdb" ]; then
    7z a -mx=7 -bd distrib/"${SYMBFILE}" game.exe custom.exe ./win32/custom.pdb ./win32/game.pdb > /dev/null
    mustexist distrib/"${SYMBFILE}"
  else
    echo "!!WARNING!! game.pdb or custom.pdb is missing - will not upload symbols file!"
  fi

  echo "Now uploading"
  scp distrib/"${ZIPFILE}" "${SCPHOST}":"${SCPDEST}"
  [ -f distrib/"${SYMBFILE}" ] && scp distrib/"${SYMBFILE}" "${SCPHOST}":"${SCPSYMBOLS}"

  echo
}

#-----------------------------------------------------------------------
# turn off wine's debug noise
export WINEDEBUG=fixme-all

svn cleanup
# Plotdict gets modified by update-html.sh, remove any modifications or conflicts
svn resolve --accept theirs-full --recursive docs
svn revert --recursive docs

svn update | tee nightly-temp.txt || exit 1
UPDATE=`grep "Updated to revision" nightly-temp.txt`
rm nightly-temp.txt

if [ -z "$UPDATE" ] ; then
  echo no changes, no need to update nightly.
  exit
fi

svn info > svninfo.txt

# Build all utilities once
${BUILD} relump unlump $SCONS_ARGS || exit 1
if [ -z "$DONT_BUILD_HSPEAK" ]; then
  ${BUILD} hspeak $SCONS_ARGS || exit 1
fi
mustexist unlump.exe
mustexist relump.exe
mustexist hspeak.exe

rm -f game*.exe custom*.exe
${BUILD} gfx=directx+sdl+fb music=sdl $SCONS_ARGS || exit 1
zip_and_upload music_sdl gfx_directx.dll SDL.dll SDL_mixer.dll

# This is the default build (default download is symlinked to it on the server)
rm -f game*.exe custom*.exe
${BUILD} gfx=sdl2+directx+fb music=sdl2 $SCONS_ARGS || exit 1
zip_and_upload sdl2 gfx_directx.dll SDL2.dll SDL2_mixer.dll

# Create the installer from the executables we just built: the installer and .zips for default build configs
# must contain the same executables, to share .pdb files
echo "InfoBeforeFile=IMPORTANT-nightly.txt" > iextratxt.txt
wine "${ISCC}" /Q /Odistrib /Fohrrpgce-win-installer ohrrpgce.iss
rm -f iextratxt.txt
mustexist "distrib/ohrrpgce-win-installer.exe"
scp -p distrib/ohrrpgce-win-installer.exe "${SCPHOST}":"${SCPDEST}"/ohrrpgce-wip-win-installer.exe

# Player-only zip
rm -f distrib/ohrrpgce-player-win-wip-sdl2.zip
zip -q -9 distrib/ohrrpgce-player-win-wip-sdl2.zip game.exe SDL2.dll SDL2_mixer.dll gfx_directx.dll LICENSE-binary.txt README-player-only.txt svninfo.txt
scp -p distrib/ohrrpgce-player-win-wip-sdl2.zip "${SCPHOST}":"${SCPDEST}"

rm -f game*.exe custom*.exe
${BUILD} music=native $SCONS_ARGS
zip_and_upload music_native gfx_directx.dll SDL2.dll audiere.dll

rm -f game*.exe custom*.exe
${BUILD} music=native2 $SCONS_ARGS
zip_and_upload music_native2 gfx_directx.dll SDL2.dll audiere.dll

rm -f game*.exe custom*.exe
${BUILD} music=silence $SCONS_ARGS
zip_and_upload music_silence gfx_directx.dll SDL2.dll

# rm -f game*.exe custom*.exe
# ${BUILD} gfx=alleg+directx+fb+sdl music=sdl $SCONS_ARGS
# zip_and_upload gfx_alleg-music_sdl alleg40.dll SDL.dll SDL_mixer.dll

rm -f game*.exe custom*.exe
${BUILD} debug=2 pdb=1
zip_and_upload sdl2-debug gfx_directx.dll SDL2.dll SDL2_mixer.dll misc/gdbcmds1.txt misc/gdbcmds2.txt gdbgame.bat gdbcustom.bat

# Note: when adding or modifying builds, BACKENDS_SYMSNAME in misc/process_crashreports.py should be updated


# Note that this is duplicated in distrib-nightly-linux.sh
echo "uploading plotscripting docs"
scp docs/*.png "${SCPHOST}":"${SCPDOCS}"
scp docs/plotdict.xml "${SCPHOST}":"${SCPDOCS}"
scp docs/htmlplot.xsl "${SCPHOST}":"${SCPDOCS}"
docs/update-html.sh
scp docs/plotdictionary.html "${SCPHOST}":"${SCPDOCS}"

rm -f distrib/ohrrpgce-util.zip
zip distrib/ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt svninfo.txt
scp distrib/ohrrpgce-util.zip "${SCPHOST}":"${SCPDEST}"

rm -f distrib/hspeak-win-nightly.zip
zip distrib/hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e euphoria/*.e euphoria/License.txt LICENSE.txt plotscr.hsd scancode.hsi
scp distrib/hspeak-win-nightly.zip "${SCPHOST}":"${SCPDEST}"

rm -f distrib/bam2mid.zip
rm -f bam2mid.exe
${BUILD} bam2mid.exe $SCONS_ARGS
mustexist bam2mid.exe
zip distrib/bam2mid.zip bam2mid.exe bam2mid.txt LICENSE.txt svninfo.txt
scp distrib/bam2mid.zip "${SCPHOST}":"${SCPDEST}"

rm -f distrib/madplay+oggenc.zip
zip distrib/madplay+oggenc.zip support/madplay.exe support/oggenc.exe support/LICENSE-{madplay,oggenc}.txt
scp distrib/madplay+oggenc.zip "${SCPHOST}":"${SCPDEST}"

scp svninfo.txt "${SCPHOST}":"${SCPDEST}"
