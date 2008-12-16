#/bin/sh

WEBDIR="/var/www/ohrimport/"
TMPDIR="./"

function mirrormedia () {
  SUBDIR=${1}
  EXT=${2}
  URL=${3}
  echo "Mirroring ${SUBDIR} from ${URL}"

  if [ ! -d "${SUBDIR}" ] ; then
    mkdir "${SUBDIR}"
  fi
  cd "${SUBDIR}"
  rm ${EXT}

  echo "Downloading..."
  httrack \
    --quiet \
    --robots=0 \
    -N "%n.%t" \
    "${URL}" \
    -"*" \
    +"${EXT}" \
    -"*/Image:*" \
    -"*/Media:*" \
    > /dev/null

  rm -R *.gif hts-* *.html
  cd ..
  zip -r import.zip "${SUBDIR}"
  rm -Rf "${SUBDIR}"
}

cd "${TMPDIR}"
rm import.zip
mirrormedia "Sound Effects" "*.ogg" "http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/Free_Sound_Effects"
mirrormedia "Music" "*.mid" "http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/Free_Music"
mv import.zip "${WEBDIR}"
