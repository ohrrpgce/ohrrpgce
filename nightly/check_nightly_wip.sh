#!/bin/bash

SCRIPT_DIR=$(dirname "$0")
cd $SCRIPT_DIR

if [ -d "./check_nightly" ] ; then
  #echo "Cleanup leftover check_nightly directory..."
  rm -Rf ./check_nightly
fi

#echo "Create new check_nightly directory"
mkdir ./check_nightly
cd ./check_nightly

for NAME in "ohrrpgce-player-win-wip-sdl2.zip" \
            "ohrrpgce-player-win-wip-win95.zip" \
            "ohrrpgce-player-linux-wip-x86.zip" \
            "ohrrpgce-player-linux-wip-x86_64.zip" \
            "ohrrpgce-player-mac-wip-x86.tar.gz" \
            "ohrrpgce-player-mac-wip-x86_64.tar.gz" ; do
  EXT="${NAME#*.}"
  BASE="${NAME%%.*}"
  echo "[$NAME]"
  mkdir ./tmp
  wget -q "https://hamsterrepublic.com/ohrrpgce/nightly/$NAME" -O "./tmp/$NAME"
  if [ "$EXT" = "zip" ] ; then
    unzip -q "./tmp/$NAME" "buildinfo.ini" -d ./
  fi
  if [ "$EXT" = "tar.gz" ] ; then
    tar -z -x "buildinfo.ini" -f "./tmp/$NAME"
  fi
  grep -e "build_date" -e "svn_rev" buildinfo.ini | sed 's/^/  /'
  rm -f buildinfo.ini
  rm -Rf ./tmp

done

