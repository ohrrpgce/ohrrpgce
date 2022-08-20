#!/bin/bash

# Identical to distrib-win.bat except it cross-compiles/packages
# from Linux, using either wine or mxe -- uncomment either section below.

set -e

echo "Building OHRRPGCE distribution for Windows from Linux"

#-----------------------------------------------------------------------

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

echo ==========================================
echo Building sdl2 executables...

# scons continues if can't create the pdb files
rm -f game.exe custom.exe relump.exe unlump.exe hspeak.exe win32/game.pdb win32/custom.pdb

# Equivalent to gfx=sdl2+directx+fb music=sdl2
$BUILD game custom buildname=sdl2 $SCONS_ARGS
# Would compile with lto=1 to reduce unlump/relump size, but that causes mingw-w64 gcc 8.1.0 to crash
$BUILD hspeak unlump relump win95=1 sse2=0 $SCONS_ARGS

echo ------------------------------------------
echo Packaging game player ohrrpgce-player-win-sdl2-*.zip ...
./ohrpackage.py win player distrib/ohrrpgce-player-win-sdl2-{TODAY}-{BRANCH}.zip

echo ------------------------------------------
echo Packaging minimal-but-complete ohrrpgce-minimal-*.zip ...
./ohrpackage.py win minimal distrib/ohrrpgce-minimal-{TODAY}-{BRANCH}.zip

echo ------------------------------------------
echo Packaging ohrrpgce-*.zip ...
./ohrpackage.py win full distrib/ohrrpgce-{TODAY}-{BRANCH}.zip

echo ------------------------------------------
echo Packaging ohrrpgce-win-installer-*.exe ...
./ohrpackage.py win full+vikings distrib/ohrrpgce-win-installer-{TODAY}-{BRANCH}.exe --iscc "$ISCC"

echo ------------------------------------------
echo Packaging sdl2 debug info archive
./ohrpackage.py win symbols distrib/ohrrpgce-symbols-win-{BUILDNAME}-{REV}-{TODAY}-{BRANCH}.7z

echo ==========================================
echo Building win95 executables...

rm -f game.exe custom.exe win32/game.pdb win32/custom.pdb
# Equivalent to gfx=directx+sdl+fb music=sdl
$BUILD game custom win95=1 sse2=0 buildname=music_sdl $SCONS_ARGS

echo ------------------------------------------
echo Packaging game player ohrrpgce-player-win-win95-*.zip ...
./ohrpackage.py win player distrib/ohrrpgce-player-win-win95-{TODAY}-{BRANCH}.zip

echo ------------------------------------------
echo Packaging ohrrpgce-win95-*.zip ...
./ohrpackage.py win full distrib/ohrrpgce-win95-{TODAY}-{BRANCH}.zip

echo ------------------------------------------
echo Packaging win95 debug info archive
./ohrpackage.py win symbols distrib/ohrrpgce-symbols-win-{BUILDNAME}-{REV}-{TODAY}-{BRANCH}.7z

echo ==========================================
echo Packaging source snapshot zip ...
./ohrpackage.py win source distrib/ohrrpgce-source-{TODAY}-{BRANCH}.zip

echo Done.
