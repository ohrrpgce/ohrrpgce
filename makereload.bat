@echo off
cls
call fbc -c -g reload.bas
if ERRORLEVEL 1 goto end
call fbc -c -g reloadext.bas
if ERRORLEVEL 1 goto end
call fbc -g -profile -lang deprecated reloadtest.bas reload.o reloadext.o util.bas -g
call fbc -g -profile -lang deprecated xml2reload.bas reload.o reloadext.o util.bas -p . -l xml2
:end