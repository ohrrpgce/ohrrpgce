

CALL distver.bat
set BUILDNAME=%1
set ZIPFILE=ohrrpgce-win-%BUILDNAME%-%OHRVERCODE%.zip
set SYMBFILE=ohrrpgce-symbols-win-%BUILDNAME%-r%SVNREV%-%OHRVERDATE%-%OHRVERCODE%.7z
echo Now creating %ZIPFILE%

for %%X in (game.exe custom.exe) do (
    if not exist "%%X" (
        ECHO "ERROR: build result %%X is missing. Unable to continue."
        GOTO failed
    )
)

support\rm -f distrib\%ZIPFILE%
support\zip -q distrib\%ZIPFILE% game.exe custom.exe hspeak.exe
support\zip -q -r distrib\%ZIPFILE% data
support\zip -q distrib\%ZIPFILE% whatsnew.txt *-binary.txt *-nightly.txt plotscr.hsd scancode.hsi svninfo.txt
support\zip -q -r distrib\%ZIPFILE% ohrhelp
support\zip -q distrib\%ZIPFILE% support\madplay.exe
support\zip -q distrib\%ZIPFILE% support\oggenc.exe
support\zip -q distrib\%ZIPFILE% support\zip.exe
support\zip -q distrib\%ZIPFILE% support\CrashRpt*.dll support\CrashSender*.exe support\crashrpt_lang.ini
copy /y relump.exe support\
support\zip -q distrib\%ZIPFILE% support\relump.exe
del support\relump.exe

IF NOT EXIST distrib\%ZIPFILE% (
    ECHO Failed to create zip
    GOTO failed
)

support\rm -rf sanity
mkdir sanity
cd sanity
..\support\unzip -qq ..\distrib\%ZIPFILE%
cd ..
IF NOT EXIST sanity\game.exe GOTO sanityfailed
IF NOT EXIST sanity\custom.exe GOTO sanityfailed
IF NOT EXIST sanity\hspeak.exe GOTO sanityfailed
support\rm -rf sanity

:addextrafiles

IF NOT EXIST "%2" GOTO extrafilesdone
support\zip -q distrib\%ZIPFILE% %2
shift
goto addextrafiles
:extrafilesdone

for %%X in (win32\game.pdb win32\custom.pdb) do (
    if not exist "%%X" (
        ECHO "!!WARNING!! %%X is missing - will not upload symbols file!"
        GOTO skipsymbols
    )
)

ECHO Now creating %SYMBFILE%
support\rm -f distrib\%SYMBFILE%
support\7za a -mx=7 -bd distrib\%SYMBFILE% game.exe custom.exe .\win32\custom.pdb .\win32\game.pdb > NUL
IF NOT EXIST distrib\%SYMBFILE% (
    ECHO 7za failed
    GOTO failed
)

pscp -q distrib\%SYMBFILE% %SCPHOST%:%SCPSYMBOLS%

:skipsymbols
pscp -q distrib\%ZIPFILE% %SCPHOST%:%SCPDEST%
GOTO finished

:sanityfailed
ECHO Sanity check failed!
support\rm -rf sanity
GOTO finished

:failed

:finished

echo.
echo.
