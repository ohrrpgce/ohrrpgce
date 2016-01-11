#!/bin/bash
#pass 'nightly' as first argument to build nightlies instead of releases

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
 cp ohrrpgce.new tmpdist
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
 mkdir tmpdist/ohrhelp
 cp ohrhelp/*.txt tmpdist/ohrhelp
 mkdir tmpdist/docs
 cp docs/*.URL tmpdist/docs
 cp docs/plotdictionary.html tmpdist/docs
 cp docs/more-docs.txt tmpdist/docs
 unix2dos -q tmpdist/*.txt tmpdist/*.hsd tmpdist/*.hsi tmpdist/support/*.txt tmpdist/docs/*.txt
}

#-----------------------------------------------------------------------
# turn of wine's debug noise
export WINEDEBUG=fixme-all

SCONS="C:\Python27\Scripts\scons.bat"
ISCC="C:\Program Files\Inno Setup 5\iscc.exe"
SVN="C:\Program Files\Subversion\bin\svn.exe"
EUC="C:\Euphoria\bin\euc.exe"

echo "Building executables..."

rm game.exe custom.exe relump.exe unlump.exe hspeak.exe

wine cmd /C "${SCONS}" game custom hspeak unlump.exe relump.exe debug=0

for exe in "game.exe" "custom.exe" "unlump.exe" "relump.exe" ; do
  mustexist "${exe}"
done

echo "Lumping Vikings of Midgard"
scons relump
rm -f vikings.rpg
./relump.exe vikings/vikings.rpgdir vikings.rpg
mustexist "vikings.rpg"

echo "Downloading import.zip"
rm -f import.zip
wget -q http://rpg.hamsterrepublic.com/ohrimport/import.zip
mustexist "import.zip"

rm -f "import/Music/"*
rm -f "import/Sound Effects/"*
echo "Unpacking import.zip"
unzip -q -d import/ import.zip

rm -f distrib/ohrrpgce-minimal.zip
rm -f distrib/ohrrpgce.zip
rm -f distrib/ohrrpgce-win-installer.exe

echo "Make temporary folder..."
rm -Rf tmpdist
mkdir tmpdist

echo "Packaging minimalist ohrrpgce-minimal.zip ..."
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
mkdir "tmpdist/Vikings script files"
cp "vikings/Vikings script files/viking.txt" "tmpdist/Vikings script files"
cp "vikings/Vikings script files/viking.hsi" "tmpdist/Vikings script files"
cp "vikings/Vikings script files/utility.lib" "tmpdist/Vikings script files"
cp "vikings/README-vikings.txt" tmpdist
unix2dos -q "tmpdist/Vikings script files/"*
# Import folder
mkdir tmpdist/import
mkdir tmpdist/import/background
cp import/background/*.bmp tmpdist/import/background
mkdir tmpdist/import/fonts
cp import/fonts/*.ohf tmpdist/import/fonts
mkdir tmpdist/import/Music
cp import/Music/*.* tmpdist/import/Music
mkdir "tmpdist/import/Sound Effects"
cp import/"Sound Effects"/*.ogg tmpdist/import/"Sound Effects"
mkdir "tmpdist/import/Master Palettes"
cp import/"Master Palettes"/*.bmp tmpdist/import/"Master Palettes"

cd tmpdist
zip -9 -q -r ../distrib/ohrrpgce.zip *
cd ..

rm -Rf tmpdist
mkdir tmpdist
cd tmpdist
unzip -q ../distrib/ohrrpgce.zip custom.exe
cd ..
mustexist "tmpdist/custom.exe"

echo "Packaging ohrrpgce-win-installer.exe ..."
if [ "${1}" = "nightly" ] ; then
  echo "InfoBeforeFile=IMPORTANT-nightly.txt" > iextratxt.txt
else
  echo "" > iextratxt.txt
fi

wine "${ISCC}" /Q /Odistrib /Fohrrpgce-win-installer ohrrpgce.iss
rm -f iextratxt.txt

mustexist "distrib/ohrrpgce-win-installer.exe"

echo "Packaging source snapshot zip ..."
OHRVERDATE=`svn info | grep "^Last Changed Date:" | cut -d ":" -f 2 | cut -d " " -f 2`
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
cd ..

echo "Done."
