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
  DIF=`diff "${i}" "${OLD}"`
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
