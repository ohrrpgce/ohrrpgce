#!/bin/sh

fbc -v -c -g -lang deprecated reload.bas reloadext.bas lumpfile.bas util.bas || exit 1
gcc -c -g -O3 base64.c --std=c99 || exit 1
gcc -c -g -O3 blit.c || exit 1

fbc -v -g -profile -lang deprecated reloadtest.bas reload.o reloadext.o lumpfile.o util.o base64.o blit.o
fbc -v -g -profile -lang deprecated xml2reload.bas reload.o reloadext.o lumpfile.o util.o base64.o blit.o -p . -l xml2
fbc -v -g -profile -lang deprecated reload2xml.bas reload.o lumpfile.o util.o base64.o blit.o
fbc -v -g -profile -lang deprecated reloadutil.bas reload.o reloadext.o lumpfile.o util.o base64.o blit.o

rm reload.o reloadext.o lumpfile.o util.o base64.o blit.o
