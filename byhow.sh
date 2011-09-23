#!/bin/sh
BAS="${1}"
if [ "${BAS}" = "" ] ; then
  echo "Specify a .bas filename on the command line"
  exit 1
fi
if [ ! -f "${BAS}" ] ; then
  echo "${BAS} must exist"
  exit 1
fi

LNUMS=`fbc -lang deprecated -c "${BAS}" -o temp.o -mt -exx -g -d DATAFILES='"/usr/share/games/ohrrpgce"' -d IS_GAME -m game -w pedantic \
  | grep "\.bas(" \
  | cut -d "(" -f 2 \
  | cut -d ")" -f 1 \
  `

for L in ${LNUMS} ; do
  head -n "${L}" "${BAS}" | tail -n 1 | sed -e s/"^"/"${L}:"/
done | grep -i \
  -e "\((\|,\) *[^ ()]\+ as integer" \
  -e "\((\|,\) *[^ ()]\+ as long" \
  -e "\((\|,\) *[^ ()]\+ as short" \
  -e "\((\|,\) *[^ ()]\+ as float" \
  -e "\((\|,\) *[^ ()]\+ as double" \
  -e "\((\|,\) *[^ ()]\+ as short" \
  -e "\((\|,\) *[^ ()]\+ as [^ ]+ ptr"
