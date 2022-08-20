#!/bin/bash

# Identical to distrib-nightly-win.bat except it cross-compiles/packages
# from Linux, using either wine or mxe, and also it updates and uploads
# docs/plotdictionary.html

echo "OHRRPGCE nightly build for Windows from Linux"
echo "---------------------------------------------------"

SCPHOST="james_paige@motherhamster.org"
SCPDEST="HamsterRepublic.com/ohrrpgce/nightly"
SCPDOCS="HamsterRepublic.com/ohrrpgce/nightly/docs"
SCPSYMBOLS="HamsterRepublic.com/ohrrpgce/symbols-archive"

SCONS_ARGS="release=1 pdb=1"

#### Using wine
export WINEDEBUG=fixme-all  # turn off wine's debug noise
SCONS="C:\Python27\Scripts\scons.bat"
BUILD="wine cmd /C ${SCONS}"
EUC="C:\Euphoria\bin\euc.exe"

#### Uncomment to cross-compile with mxe
# export PATH=~/src/mxe/usr/bin:$PATH
# BUILD="scons target=i686-w64-mingw32.static"
# # To cross-compile hspeak, need two Euphoria installations
# BUILD+=" eulib=~/local/euphoria-4.1.0-Windows-x86/bin/eu.a"
# export EUDIR=~/local/euphoria-4.1.0-Linux-x64/
# export EUC=$EUDIR/bin/euc

# Find iscc.exe
ISCC='C:\Program Files\Inno Setup 5\iscc.exe'
if ! [ -f "$(winepath "$ISCC")" ]; then
    ISCC='C:\Program Files (x86)\Inno Setup 5\iscc.exe'
    if ! [ -f "$(winepath "$ISCC")" ]; then
        echo "Can't find Inno Setup 5"
        exit 1
    fi
fi

OHRVERDATE=`svn info | grep "^Last Changed Date:" | cut -d ":" -f 2 | cut -d " " -f 2`
SVNREV=`svn info | grep "^Revision:" | cut -d " " -f 2`

#-----------------------------------------------------------------------

function zip_and_upload {
  BUILDNAME="${1}"
  ZIPFILE="ohrrpgce-win-${BUILDNAME}-wip.zip"
  SYMBFILE="ohrrpgce-symbols-win-${BUILDNAME}-r${SVNREV}-${OHRVERDATE}-wip.7z"
  echo "    Packaging $BUILDNAME nightly"

  ./ohrpackage.py win nightly "distrib/$ZIPFILE" -- "${@:2}" && {

    echo "    Packaging $BUILDNAME symbols"
    if ! ./ohrpackage.py win symbols "distrib/$SYMBFILE"; then
      echo "Skipping $BUILDNAME nightly because missing symbols!"
    else
      echo "Now uploading"
      scp distrib/"${ZIPFILE}" "${SCPHOST}":"${SCPDEST}"
      scp distrib/"${SYMBFILE}" "${SCPHOST}":"${SCPSYMBOLS}"
    fi
  }
  echo
}

#-----------------------------------------------------------------------

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

#-----------------------------------------------------------------------


# Build all utilities once
# Would compile with lto=1 to reduce unlump/relump size, but that causes mingw-w64 gcc 8.1.0 to crash
$BUILD hspeak relump unlump win95=1 sse2=0 $SCONS_ARGS || exit 1

# This is the build for obsolete Windows machines (symlinked as ohrrpgce-win-win95-wip.zip)
$BUILD gfx=directx+sdl+fb music=sdl win95=1 sse2=0 buildname=music_sdl $SCONS_ARGS && {
  zip_and_upload music_sdl

  echo "    Packaging win95 game player ..."
  ./ohrpackage.py win player distrib/ohrrpgce-player-win-win95-wip.zip &&
    scp -p distrib/ohrrpgce-player-win-win95-wip.zip "${SCPHOST}":"${SCPDEST}"
}

# This is the default build (default download ohrrpgce-win-default.zip is symlinked to it on the server)
$BUILD gfx=sdl2+directx+fb music=sdl2 buildname=sdl2 $SCONS_ARGS && {
  zip_and_upload sdl2

  echo "    Packaging ohrrpgce-win-installer-wip.exe ..."
  # Create the installer from the executables we just built: the installer and .zips for default build configs
  # must contain the same executables, to share .pdb files
  ./ohrpackage.py win full+vikings distrib/ohrrpgce-win-installer-wip.exe --iscc "$ISCC" &&
    scp -p distrib/ohrrpgce-win-installer-wip.exe "${SCPHOST}":"${SCPDEST}"

  echo "    Packaging sdl2 game player ..."
  ./ohrpackage.py win player distrib/ohrrpgce-player-win-sdl2-wip.zip &&
    scp -p distrib/ohrrpgce-player-win-sdl2-wip.zip "${SCPHOST}":"${SCPDEST}"

}

$BUILD music=native buildname=music_native $SCONS_ARGS &&
  zip_and_upload music_native

$BUILD music=native2 buildname=music_native2 $SCONS_ARGS &&
  zip_and_upload music_native2

$BUILD music=silence buildname=music_silence $SCONS_ARGS &&
  zip_and_upload music_silence

$BUILD debug=2 pdb=1 buildname=sdl2-debug &&
  zip_and_upload sdl2-debug misc/gdbcmds1.txt gdbgame.bat gdbcustom.bat


echo "    Packaging other utilities"

# Note that this is duplicated in distrib-nightly-linux.sh
echo "uploading plotscripting docs"
scp docs/*.png "${SCPHOST}":"${SCPDOCS}"
scp docs/plotdict.xml "${SCPHOST}":"${SCPDOCS}"
scp docs/htmlplot.xsl "${SCPHOST}":"${SCPDOCS}"
# distrib-nightly-win.bat doesn't do this
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
${BUILD} bam2mid.exe $SCONS_ARGS && {
  zip distrib/bam2mid.zip bam2mid.exe bam2mid.txt LICENSE.txt svninfo.txt
  scp distrib/bam2mid.zip "${SCPHOST}":"${SCPDEST}"
}

rm -f distrib/madplay+oggenc.zip
zip distrib/madplay+oggenc.zip support/madplay.exe support/oggenc.exe support/LICENSE-{madplay,oggenc}.txt
scp distrib/madplay+oggenc.zip "${SCPHOST}":"${SCPDEST}"

scp svninfo.txt "${SCPHOST}":"${SCPDEST}"
