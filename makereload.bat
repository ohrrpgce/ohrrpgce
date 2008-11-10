@echo off
call fbc -g reloadtest.bas reload.bas
call fbc -g xml2reload.bas reload.bas -p . -l xml2