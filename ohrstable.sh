#!/bin/sh

# James runs this script on HamsterRepublic.com to update the symbolic links for the latest stable release

WEBROOT=~/HamsterRepublic.com
ARCHIVE="${WEBROOT}"/ohrrpgce/archive
DL="${WEBROOT}"/dl
REL="../ohrrpgce/archive"

STABLE=`
ls -l "${DL}"/ohrrpgce-win-installer.exe \
  | sed -e s/".*\/"/""/ \
        -e s/"ohrrpgce-win-installer-"/""/ \
        -e s/"\.exe$"/""/ \
  | cut -d "-" -f 4- \
  `
echo "Current stable milestone is: ${STABLE}"

VER="${1}"
USAGE="true"

if [ -e "$ARCHIVE"/ohrrpgce-win-installer-????-??-??-"${VER}".exe ] ; then
  unset USAGE
fi

if [ "${USAGE}" ] ; then
  if [ -z "${VER}" ] ; then
    echo "You must specify a milestone on the command-line."
  else
    echo "Milestone \"${VER}\" is not valid."
  fi
  echo "Available milestones:"
  ls -1 "${ARCHIVE}"/ohrrpgce-win-installer-*.exe \
    | sed -e s/"^.*\/ohrrpgce-win-installer-"/""/ \
          -e s/"\.exe$"/""/ \
    | sort \
    | uniq \
    | cut -d "-" -f "4-"
  exit
fi

echo "Updating links to point to ${VER} milestone..."
cd "${DL}"

function sourcefile () {
  REL="${1}"
  PREFIX="${2}"
  VER="${3}"
  EXT="${4}"
  ls -1 "${REL}/${PREFIX}"-????-??-??-"${VER}${EXT}" \
         2>&1 \
         | grep -v ": No such file or directory" \
         | sed s/".*\/"/""/
}

function updatelink () {
  REL="${1}"
  VER="${2}"
  PREFIX="${3}"
  EXT="${4}"
  OLDPREFIX="${5}"
  OLDPREFIX2="${6}"
  DFILE="${PREFIX}${EXT}"
  printf "  ${DFILE}"
  SFILE=`sourcefile "${REL}" "${PREFIX}" "${VER}" "${EXT}"`
  if [ ! -f "${REL}/${SFILE}" ] ; then
    if [ -n "${OLDPREFIX}" ] ; then
      SFILE=`sourcefile "${REL}" "${OLDPREFIX}" "${VER}" "${EXT}"`
      if [ ! -f "${REL}/${SFILE}" ] ; then
        if [ -n "${OLDPREFIX2}" ] ; then
          SFILE=`sourcefile "${REL}" "${OLDPREFIX2}" "${VER}" "${EXT}"`
          if [ -n "${OLDPREFIX2}" ] ; then
            printf " (NONE OF 3 FOUND!)\n"
            exit
          fi
        else
          printf " (NEITHER FOUND!)\n"
          exit
        fi
      fi
    else
      printf " (NOT FOUND!)\n"
      exit
    fi
  fi
  printf " (${SFILE})"
  rm "${DFILE}"
  ln -s "${REL}/${SFILE}" "${DFILE}"
  printf "\n"
}

# Windows files
updatelink "${REL}" "${VER}" "ohrrpgce-win-installer" ".exe" "" ""
updatelink "${REL}" "${VER}" "ohrrpgce"               ".zip" "custom" ""
updatelink "${REL}" "${VER}" "ohrrpgce-minimal"       ".zip" "ohrrpgce-floppy" "ohrrpgce_play"

# Old Mac files for versions <= etheldreme
#updatelink "${REL}" "${VER}" "OHRRPGCE"               ".dmg" "" ""
#updatelink "${REL}" "${VER}" "ohrrpgce-mac-minimal"   ".tar.gz" "" ""

# New Mac files >= fufluns
updatelink "${REL}" "${VER}" "OHRRPGCE"               "-x86_64.dmg" "" ""
updatelink "${REL}" "${VER}" "ohrrpgce-mac-minimal"   "-x86_64.tar.gz" "" ""
updatelink "${REL}" "${VER}" "OHRRPGCE"               "-x86.dmg" "" ""
updatelink "${REL}" "${VER}" "ohrrpgce-mac-minimal"   "-x86.tar.gz" "" ""

# Android files
updatelink "${REL}" "${VER}" "ohrrpgce-game-android-debug" ".apk" "" ""

# Old Linux files <= callipygous
# Uncomment these and comment the others if you need to roll back to an old stable for no plausible reason I can imagine
#updatelink "${REL}" "${VER}" "ohrrpgce-linux-x86"     ".tar.bz2" "" ""
#updatelink "${REL}" "${VER}" "ohrrpgce-player-linux-bin-minimal" ".zip" "" ""

# New Linux files >= dwimmercrafty
updatelink "${REL}" "${VER}" "ohrrpgce-linux"     "-x86.tar.bz2" "" ""
updatelink "${REL}" "${VER}" "ohrrpgce-linux"     "-x86_64.tar.bz2" "" ""
updatelink "${REL}" "${VER}" "ohrrpgce-player-linux-bin-minimal" "-x86.zip" "" ""
updatelink "${REL}" "${VER}" "ohrrpgce-player-linux-bin-minimal" "-x86_64.zip" "" ""

# Source code
updatelink "${REL}" "${VER}" "ohrrpgce-source"    ".zip" "" ""
