IF NOT EXIST game.exe GOTO failed
IF NOT EXIST custom.exe GOTO failed

set ZIPFILE=ohrrpgce-win-%1-wip.zip
echo Now creating and uploading %ZIPFILE%

support\rm -f distrib\%ZIPFILE%
support\zip -q distrib\%ZIPFILE% game.exe custom.exe hspeak.exe
support\zip -q -r distrib\%ZIPFILE% data
support\zip -q distrib\%ZIPFILE% whatsnew.txt *-binary.txt *-nightly.txt plotscr.hsd scancode.hsi svninfo.txt
support\zip -q -r distrib\%ZIPFILE% ohrhelp
support\zip -q distrib\%ZIPFILE% support\madplay.exe
support\zip -q distrib\%ZIPFILE% support\oggenc.exe
support\zip -q distrib\%ZIPFILE% support\zip.exe
copy /y relump.exe support\
support\zip -q distrib\%ZIPFILE% support\relump.exe
del support\relump.exe

IF NOT EXIST distrib\%ZIPFILE% GOTO failed

support\rm -rf sanity
mkdir sanity
cd sanity
..\support\unzip -qq ..\distrib\%ZIPFILE%
cd ..
IF NOT EXIST sanity\game.exe GOTO sanityfailed
IF NOT EXIST sanity\custom.exe GOTO sanityfailed
IF NOT EXIST sanity\hspeak.exe GOTO sanityfailed
support\rm -r sanity

:addextrafiles

IF NOT EXIST "%2" GOTO extrafilesdone
support\zip -q distrib\%ZIPFILE% %2
shift
goto addextrafiles
:extrafilesdone

pscp -q distrib\%ZIPFILE% james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
GOTO finished

:sanityfailed
del sanity\*.exe
del sanity\*.dll
del sanity\*.txt
del sanity\*.hsd
support\rm -r sanity

:failed

:finished

echo.
echo.
