@echo off
IF NOT EXIST game.exe GOTO failed
IF NOT EXIST custom.exe GOTO failed
echo Now uploading the OHR with %1 graphics modules, and %2 music module
REM %3 is a suffix for the zip file
set ZIPFILE=ohrrpgce-wip-%1-%2%3.zip
IF "%3"=="~" set ZIPFILE=ohrrpgce-wip-%1-%2.zip

del distrib\%ZIPFILE%
support\zip -q distrib\%ZIPFILE% game.exe custom.exe
support\zip -q distrib\%ZIPFILE% ohrrpgce.new
support\zip -q distrib\%ZIPFILE% whatsnew.txt *-binary.txt *-nightly.txt plotscr.hsd svninfo.txt
support\zip -q -r distrib\%ZIPFILE% ohrhelp

IF NOT EXIST distrib\%ZIPFILE% GOTO failed

mkdir sanity
cd sanity
..\support\unzip -qq ..\distrib\%ZIPFILE%
cd ..
IF NOT EXIST sanity\game.exe GOTO sanityfailed
IF NOT EXIST sanity\custom.exe GOTO sanityfailed
support\rm -r sanity

:addextrafiles

IF NOT EXIST "%4" GOTO extrafilesdone
support\zip -q distrib\%ZIPFILE% %4
shift
goto addextrafiles
:extrafilesdone

pscp -i C:\progra~1\putty\id_rsa.ppk distrib\%ZIPFILE% james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
GOTO finished

:sanityfailed
del sanity\*.exe
del sanity\*.dll
del sanity\*.txt
del sanity\*.hsd
rmdir sanity

:failed

:finished
