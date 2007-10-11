#/bin/sh

URL="http://HamsterRepublic.com/ohrrpgce/nightly/"
WEBDIR="/var/www/nightly-archive"

function followlink () {
  LINK=`readlink "${1}"`
  if [ -z "${LINK}" ] ; then
    echo "${1}"
  else
    followlink "${LINK}"
  fi
}

if [ ! -d "${WEBDIR}" ] ; then
  echo "Error: ${WEBDIR} does not exist"
  exit 1
fi

cp -p index.php "${WEBDIR}"

NOW=`date "+%Y-%m-%d"`

echo "Mirroring ${URL} on ${NOW}"

cd "${WEBDIR}"
if [ -d "${NOW}" ] ; then
  echo "Oops! we have already mirrored today."
  exit 1
fi
mkdir "${NOW}"
cd "${NOW}"

echo "Downloading..."
httrack \
  --quiet \
  --robots=0 \
  -N "%n.%t" \
  "${URL}" \
  -"*" \
  +"*.zip" \
  -"*-default.zip" \
  > /dev/null

rm -R *.gif hts-* *.html

COUNT=`ls -1 *.zip | wc -l`
if [ ${COUNT} -eq 0 ] ; then
  echo "No files downloaded."
  cd ..
  rmdir "${NOW}"
  exit 1
fi

YEST=`date -d "Yesterday" "+%Y-%m-%d"`
YDIR="../${YEST}"

for i in *.zip ; do
  OLD="${YDIR}/${i}"
  if [ ! -f "${OLD}" ] ; then
    echo "${i} (NEW)"
    continue
  fi
  TMP1="/tmp/ohrnightly.${RANDOM}.tmp"
  TMP2="/tmp/ohrnightly.${RANDOM}.tmp"
  unzip -qq -d "${TMP1}" "${i}"
  unzip -qq -d "${TMP2}" "${OLD}"
  if [ -f "${TMP1}/svninfo.txt" -a -f "${TMP2}/svninfo.txt" ] ; then
    DIF=`diff -u "${TMP1}/svninfo.txt" "${TMP2}/svninfo.txt"`
  else
    DIF=`diff -r "${TMP1}" "${TMP2}"`
  fi
  rm -Rf "${TMP1}" "${TMP2}"
  if [ "${DIF}" ] ; then
    echo "${i} (Updated)"
  else
    printf "${i}"
    LINK=`followlink "${OLD}"`
    if [ -f "${LINK}" ] ; then
      rm "${i}"
      ln -s "${LINK}" "${i}"
      echo " (${LINK})"
    else
      echo " (unable to symlink)"
    fi
  fi
done

# Throw away nightlies older than one year.
find "${WEBDIR}" -type d -mtime +365 -exec rm -R "{}" \;
