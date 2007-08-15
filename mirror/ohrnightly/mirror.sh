#/bin/sh

URL="http://HamsterRepublic.com/ohrrpgce/nightly/"
WEBDIR="/var/www/nightly-archive"
NOW=`date "+%Y-%m-%d"`

echo "Mirroring ${URL} on ${NOW}"

if [ -d "${NOW}" ] ; then
  echo "Oops! we have already mirrored today."
  exit 1
fi

cd "${WEBDIR}"
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
    echo "${i} (Not changed)"
    rm "${i}"
    ln -s "${OLD}" "${i}"
  fi
done

# Throw away nightlies older than one year.
find "${WEBDIR}" -type d -mtime +365 -exec rm -R "{}" \;
