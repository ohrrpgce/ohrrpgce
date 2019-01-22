#!/bin/bash

echo "Building OHRRPGCE distribution for Windows using Linux+Wine"
echo "-----------------------------------------------------------"

#-----------------------------------------------------------------------

function mustexist {
  if [ ! -f "${1}" -a ! -d "${1}" ] ; then
    echo "ERROR: ${1} does not exist!"
    exit 1
  fi
}  

function ohrrpgce_common_files {
 cp game.exe tmpdist
 cp custom.exe tmpdist
 cp hspeak.exe tmpdist
 cp plotscr.hsd tmpdist
 cp scancode.hsi tmpdist
 cp README-game.txt tmpdist
 cp README-custom.txt tmpdist
 cp IMPORTANT-nightly.txt tmpdist
 cp LICENSE.txt tmpdist
 cp LICENSE-binary.txt tmpdist
 cp SDL.dll tmpdist
 cp SDL_mixer.dll tmpdist
 cp gfx_directx.dll tmpdist
 mkdir tmpdist/support
 cp support/madplay.exe tmpdist/support
 cp support/LICENSE-madplay.txt tmpdist/support
 cp support/oggenc.exe tmpdist/support
 cp support/LICENSE-oggenc.txt tmpdist/support
 cp support/wget.exe tmpdist/support
 cp support/wget.hlp tmpdist/support
 cp support/zip.exe tmpdist/support
 cp support/CrashRpt*.dll support/CrashSender*.exe support/crashrpt_lang.ini tmpdist/support
 cp support/unzip.exe tmpdist/support
 cp support/rcedit.exe tmpdist/support
 cp support/LICENSE-rcedit.txt tmpdist/support
 cp relump.exe tmpdist/support
 cp unlump.exe tmpdist/support
 cp -r data tmpdist/data
 cp -r ohrhelp tmpdist/ohrhelp
 mkdir tmpdist/docs
 cp docs/*.URL tmpdist/docs
 cp docs/plotdictionary.html tmpdist/docs
 cp docs/more-docs.txt tmpdist/docs
 unix2dos -q tmpdist/*.txt tmpdist/*.hsd tmpdist/*.hsi tmpdist/support/*.txt tmpdist/docs/*.txt
}

#-----------------------------------------------------------------------
# turn off wine's debug noise
export WINEDEBUG=fixme-all

SCONS="C:\Python27\Scripts\scons.bat"
ISCC="C:\Program Files\Inno Setup 5\iscc.exe"
SVN="C:\Program Files\Subversion\bin\svn.exe"
EUC="C:\Euphoria\bin\euc.exe"

SCONS_ARGS="release=1 pdb=1"

# This should be the BUILDNAME matching $SCONS_ARGS, i.e. the default backends
BUILDNAME=music_sdl

# Using wine
BUILD="wine cmd /C ${SCONS}"

# Uncomment to cross-compile
#export PATH=~/src/mxe/usr/bin:$PATH
#BUILD="scons target=i686-w64-mingw32.shared"
#DONT_BUILD_HSPEAK=yes  #TODO: scons doesn't support cross-compiling hspeak yet


echo "Building executables..."

rm -f game.exe custom.exe relump.exe unlump.exe
${BUILD} game custom unlump relump $SCONS_ARGS || exit 1

if [ -z "$DONT_BUILD_HSPEAK" ]; then
  rm -f hspeak.exe
  ${BUILD} hspeak $SCONS_ARGS || exit 1
fi

for exe in "game.exe" "custom.exe" "unlump.exe" "relump.exe" "hspeak.exe" ; do
  mustexist "${exe}"
done

echo "Lumping Vikings of Midgard"
#scons $SCONS_ARGS relump
rm -f vikings.rpg
wine ./relump.exe vikings/vikings.rpgdir vikings.rpg
mustexist "vikings.rpg"

rm -f distrib/ohrrpgce-minimal.zip
rm -f distrib/ohrrpgce.zip
rm -f distrib/ohrrpgce-win-installer.exe

echo "Make temporary folder..."
rm -Rf tmpdist
mkdir tmpdist

echo "Packaging minimalist ohrrpgce-minimal.zip ..."
# Note: this does not match contents of ohrrpgce-minimal.zip created
# by distrib.bat, which e.g. excludes all support/ utils except wget.
ohrrpgce_common_files

cd tmpdist
zip -9 -q -r ../distrib/ohrrpgce-minimal.zip *
cd ..

echo "  verify minimalist zip file..."
rm -Rf tmpdist
mkdir tmpdist
cd tmpdist
unzip -q ../distrib/ohrrpgce-minimal.zip game.exe
cd ..
mustexist "tmpdist/game.exe"

echo "Packaging ohrrpgce.zip ..."
rm -Rf tmpdist
mkdir tmpdist
ohrrpgce_common_files
# extra docs
cp docs/plotdict.xml tmpdist/docs
cp docs/htmlplot.xsl tmpdist/docs
# Vikings
cp vikings.rpg tmpdist
cp -r "vikings/Vikings script files" "tmpdist/Vikings script files"
cp "vikings/README-vikings.txt" tmpdist
unix2dos -q "tmpdist/Vikings script files/"*
# Import folder
cp -r import tmpdist/import

cd tmpdist
zip -9 -q -r ../distrib/ohrrpgce.zip *
cd ..

echo "Sanity checking ohrrpgce.zip"
rm -Rf tmpdist
mkdir tmpdist
cd tmpdist
unzip -q ../distrib/ohrrpgce.zip custom.exe
cd ..
mustexist "tmpdist/custom.exe"

echo "Packaging ohrrpgce-win-installer.exe ..."
echo "" > iextratxt.txt
wine "${ISCC}" /Q /Odistrib /Fohrrpgce-win-installer ohrrpgce.iss
rm -f iextratxt.txt

mustexist "distrib/ohrrpgce-win-installer.exe"

echo "Packaging debug info archive"
rm -f distrib/ohrrpgce-symbols-win.7z
# The ./ path prefixes add the files with the win32/ relative path
7z a -mx=7 -bd distrib/ohrrpgce-symbols-win.7z game.exe custom.exe ./win32/custom.pdb ./win32/game.pdb > /dev/null
mustexist "distrib/ohrrpgce-symbols-win.7z"

echo "Packaging source snapshot zip ..."
OHRVERDATE=`svn info | grep "^Last Changed Date:" | cut -d ":" -f 2 | cut -d " " -f 2`
SVNREV=`svn info | grep "^Revision:" | cut -d " " -f 2`
OHRVERCODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
REPOSITORY=`svn info | grep "^URL:" | cut -d " " -f 2-`
rm -Rf tmpdist
mkdir tmpdist

cd tmpdist
echo "  Checkout..."
svn co -q "${REPOSITORY}"
[ ! -d $OHRVERCODE ] && echo "Directory $OHRVERCODE is missing. Maybe codename.txt wasn't updated?" && exit 1
svn info $OHRVERCODE > $OHRVERCODE/svninfo.txt
echo "  Zip..."
zip -q -r ../distrib/ohrrpgce-source.zip *
cd ..

echo "Cleaning up..."
rm -Rf tmpdist

echo "Rename results..."
SUFFIX="${OHRVERDATE}-${OHRVERCODE}"
echo "${SUFFIX}"
cd distrib
mv ohrrpgce-minimal.zip       ohrrpgce-minimal-"${SUFFIX}".zip
mv ohrrpgce.zip               ohrrpgce-"${SUFFIX}".zip
mv ohrrpgce-win-installer.exe ohrrpgce-win-installer-"${SUFFIX}".exe
mv ohrrpgce-source.zip        ohrrpgce-source-"${SUFFIX}".zip
mv ohrrpgce-symbols-win.7z    ohrrpgce-symbols-win-"${BUILDNAME}-r${SVNREV}-${SUFFIX}".7z
cd ..

echo "Done."
