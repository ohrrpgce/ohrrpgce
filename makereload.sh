#!/bin/sh

fbc -v -c -g -lang deprecated reload.bas reloadext.bas lumpfile.bas util.bas || exit 1

fbc -v -g -profile -lang deprecated reloadtest.bas reload.o reloadext.o lumpfile.o util.o
fbc -v -g -profile -lang deprecated xml2reload.bas reload.o reloadext.o lumpfile.o util.o -p . -l xml2
fbc -v -g -profile -lang deprecated reload2xml.bas reload.o lumpfile.o util.o

rm reload.o reloadext.o lumpfile.o util.o
