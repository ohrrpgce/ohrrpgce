#!/bin/bash

# This script is meant to be run AFTER a stable release has been
# completed, and after script/ohrstable.sh has been run to update
# the stable links on HamsterRepublic.com
#
# Note that it really doesn't matter if you run this script from the
# wip branch or a release branch, it will always just upload the latest
# stable release.

# Start from this script's dir
SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"

mkdir -p itch_upload_dir
cd itch_upload_dir

if [ ! -f butler ] ; then
  echo "Installing itch.io butler..."
  curl -L -s -o butler.zip https://broth.itch.ovh/butler/linux-amd64/LATEST/archive/default
  unzip butler.zip
  rm butler.zip
  chmod +x butler
fi

CHECK=$( ./butler -V 2>&1 | grep -P -e"^v\d{2}\." )
if [ -z "$CHECK" ] ; then
  echo "Running butler didn't work"
  exit 1
else
  echo "Butler can run okay"
  echo $CHECK
fi

./butler upgrade --assume-yes

if [ ! -f ~/.config/itch/butler_creds ] ; then
  ./butler login
else
  echo "Already logged in to butler"
fi

echo "Fetching OHRRPGCE stable release files"
rm -Rf ./releases
mkdir -p releases
curl -L -s -o releases/ohrrpgce.zip https://hamsterrepublic.com/dl/ohrrpgce.zip
curl -L -s -o releases/OHRRPGCE-x86_64.dmg https://hamsterrepublic.com/dl/OHRRPGCE-x86_64.dmg
curl -L -s -o releases/ohrrpgce-linux-x86_64.tar.bz2 https://hamsterrepublic.com/dl/ohrrpgce-linux-x86_64.tar.bz2
# We don't upload the last one, it is just for buildinfo.ini
curl -L -s -o releases/ohrrpgce-player-linux-bin-minimal-x86_64.zip https://hamsterrepublic.com/dl/ohrrpgce-player-linux-bin-minimal-x86_64.zip

echo "Parsing release name"
rm -f releases/buildinfo.ini
unzip -q releases/ohrrpgce-player-linux-bin-minimal-x86_64.zip buildinfo.ini -d ./releases/
if [ ! -f "releases/buildinfo.ini" ] ; then
  echo "Unable to find buildinfo.ini"
  exit 1
fi
BUILD_DATE=$( grep "^build_date=" "releases/buildinfo.ini" | cut -d "=" -f 2 )
SVN_REV=$( grep "^svn_rev=" "releases/buildinfo.ini" | cut -d "=" -f 2 )
CODE_NAME=$( grep -a "^code_name=" "releases/buildinfo.ini" | cut -d "=" -f 2 )
VERSION_NAME="${CODE_NAME}_${BUILD_DATE}.${SVN_REV}"
echo $VERSION_NAME | iconv -f ISO-8859-1 -t UTF-8 - > buildnumber.txt
echo "Found version $( cat buildnumber.txt )"

echo "Uploading release to bob-the-hamster.itch.io..."
echo "The ohrrpgce game project should already exist..."
./butler push releases/ohrrpgce.zip bob-the-hamster/ohrrpgce:windows-stable --userversion-file buildnumber.txt
./butler push releases/OHRRPGCE-x86_64.dmg bob-the-hamster/ohrrpgce:mac-stable --userversion-file buildnumber.txt
./butler push releases/ohrrpgce-linux-x86_64.tar.bz2 bob-the-hamster/ohrrpgce:linux-stable --userversion-file buildnumber.txt
