#!/bin/sh

fbc -v -c -g reload.bas || exit 1

fbc -v -c -g reloadext.bas || exit 1

fbc -v -g -profile -lang deprecated reloadtest.bas reload.o reloadext.o util.bas
fbc -v -g -profile -lang deprecated xml2reload.bas reload.o reloadext.o util.bas -p . -l xml2

rm reload.o reloadext.o
