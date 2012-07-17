#!/bin/sh
# Make a linkless version of the minimal Mac OHRRPGCE tarball

NORMAL=ohrrpgce-mac-minimal.tar.gz
LINKLESS=ohrrpgce-mac-minimal-linkless.tar.gz

for DLDIR in ~/HamsterRepublic.com/dl ~/HamsterRepublic.com/ohrrpgce/nightly ; do

  cd ~/tmp
  mkdir linkless.tmp
  cd linkless.tmp
  echo "Unpack the normal tarball..."
  tar -zxhf "${DLDIR}/${NORMAL}"
  printf "\nThis version has no symlinks, which is needed if you\nare building Mac app bundles on a Windows computer.\n" >> README-mac-minimal.txt

  echo "Pack the linkless tarball"
  rm "${DLDIR}/${LINKLESS}"
  tar -zchf "${DLDIR}/${LINKLESS}" OHRRPGCE-Game.app LICENSE-binary.txt README-mac-minimal.txt

  cd ..
  rm -R linkless.tmp
  ls -lH "${DLDIR}/${NORMAL}" "${DLDIR}/${LINKLESS}"

done

echo "Done."
