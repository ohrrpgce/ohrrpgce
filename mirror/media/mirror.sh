#/bin/sh

WEBDIR="./"
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
    > /dev/null

  rm -R *.gif hts-* *.html
  cd ..
}

cd "${TMPDIR}"
mirrormedia "Sound Effects" "*.ogg" "http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/Free_Sound_Effects"
mirrormedia "Music" "*.mid" "http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/Free_Music"
