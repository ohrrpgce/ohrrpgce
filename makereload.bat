@echo off
call fbc -g reloadtest.bas reload.bas util.bas -g
call fbc -g xml2reload.bas reload.bas util.bas -p . -l xml2