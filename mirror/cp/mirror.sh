#/bin/sh

URL="http://castleparadox.com/search-gamelist.php?mirror=true"

rm -R "castleparadox.com" index.html date.txt url.txt > /dev/null 2>&1
rm -R mirror.tar.bz2 > /dev/null 2>&1

echo "Mirroring ${URL}" 
httrack \
  --quiet \
  --robots=0 \
  -N "%h%p/%n.%t" \
  "${URL}" \
  -"*.php" \
  +"*/download.php?game=*" \
  +"*/gamelist-display.php" \
  +"*/search-gamelist.php?letter=*" \
  +"*.gif" \
  +"*.png" \
  +"*.jpg" \
  +"*.jpeg" \
  +"*.zip" \
  -*month=* \
  -*year=* \
  -"*pages.stern.nyu.edu*" \
  -"*www.ohrrpgce.com*" \
  > /dev/null

rm backblue.gif fade.gif cookies.txt hts-log.txt > /dev/null 2>&1
rm -R hts-cache > /dev/null 2>&1

# sanity check

GAMES=`ls castleparadox.com/*.zip | wc -l`
echo "Downloaded ${GAMES} games"
if [ ${GAMES} -lt 100 ] ; then
  echo "SANITY CHECK FAILURE: where did all the games go?"
  exit 1
fi

SIZE=`du -s | cut -f 1`
echo "Mirrored ${SIZE} k"
if [ ${SIZE} -lt 150000 ] ; then
  echo "SANITY CHECK FAILURE: too little data"
  exit 1
fi

date "+%Y-%m-%d" > date.txt
echo "${URL}" | cut -d "?" -f "1" > url.txt

echo "Uploading..."
ssh james_paige@motherhamster.org mkdir mirror.motherhamster.org/cp.new && \
scp -pr ./* james_paige@motherhamster.org:mirror.motherhamster.org/cp.new/ && \
ssh james_paige@motherhamster.org mv mirror.motherhamster.org/cp mirror.motherhamster.org/cp.old && \
ssh james_paige@motherhamster.org mv mirror.motherhamster.org/cp.new mirror.motherhamster.org/cp && \
ssh james_paige@motherhamster.org rm -R mirror.motherhamster.org/cp.old && \
ssh james_paige@motherhamster.org mirror.motherhamster.org/cp.support/refresh.sh
