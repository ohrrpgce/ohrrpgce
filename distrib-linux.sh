#!/bin/sh
#
# Build and package builds for linux

SCONS_ARGS="release=1 v=1"
FULLNAME=${1:-ohrrpgce-linux-\{DATE\}-\{BRANCH\}}
PLAYERNAME=${2:-ohrrpgce-player-linux-\{DATE\}-\{BRANCH\}}

SCRIPT_DIR="${0%/*}"
SCRIPT_DIR="$(realpath $SCRIPT_DIR)"
cd "${SCRIPT_DIR}"

echo Erasing old distribution files
mkdir -p distrib
rm -f distrib/$FULLNAME-*.tar.bz2
rm -f distrib/$PLAYERNAME-*.zip
rm -f distrib/*.deb

package_for_arch() {
  ARCH=$1

  echo
  echo "Building $ARCH binaries"
  scons $SCONS_ARGS arch=$ARCH unlump relump || return 1
  scons $SCONS_ARGS arch=$ARCH libs=linux/$ARCH game custom || return 1
  scons $SCONS_ARGS release=0 arch=$ARCH libs=linux/$ARCH game custom hspeak || return 1

  echo "Packaging $ARCH binary distribution of CUSTOM"
  ./ohrpackage.py linux full "distrib/$FULLNAME-$ARCH.tar.bz2" || return 1

  echo "Prepare minimal $ARCH player zip"
  ./ohrpackage.py linux player "distrib/$PLAYERNAME-$ARCH.zip" || return 1
}

if [ -z "${OHR_SKIP_X86}" ] ; then
  package_for_arch x86 || exit 1
fi

if [ -z "${OHR_SKIP_X86_64}" ] ; then
  package_for_arch x86_64 || exit 1
  if which dpkg > /dev/null; then
    echo
    echo "Building x86_64 Debian/Ubuntu packages"
    linux/linuxpkg.py distrib || exit 1
  fi
fi
