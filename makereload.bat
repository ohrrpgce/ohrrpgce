@echo off
call fbc -c -g reload.bas
if ERRORLEVEL 1 goto end
call fbc -g reloadtest.bas reload.o util.bas -g
call fbc -g xml2reload.bas reload.o util.bas -p . -l xml2
:end