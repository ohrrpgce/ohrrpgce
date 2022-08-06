CALL distrib-win-setup.bat || exit /b 1

REM ------------------------------------------
ECHO Building executables...

REM This should be the BUILDNAME (as in distrib-nightly-win-packnupload.bat) for the default backends
set BUILDNAME=sdl2

support\rm -f game.exe custom.exe relump.exe unlump.exe hspeak.exe win32\game.pdb win32\custom.pdb

CALL scons game custom hspeak %SCONS_ARGS% || exit /b 1
REM Would compile with lto=1 to reduce unlump/relump size, but that causes mingw-w64 gcc 8.1.0 to crash
CALL scons unlump relump %SCONS_ARGS% || exit /b 1

ECHO ------------------------------------------
ECHO Packaging game player ohrrpgce-player-win-sdl2-*.zip ...
python ohrpackage.py win player distrib\ohrrpgce-player-win-sdl2-{TODAY}-{BRANCH}.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging minimal-but-complete ohrrpgce-minimal-*.zip ...
python ohrpackage.py win minimal distrib\ohrrpgce-minimal-{TODAY}-{BRANCH}.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging ohrrpgce-*.zip ...
python ohrpackage.py win full distrib\ohrrpgce-{TODAY}-{BRANCH}.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging ohrrpgce-win-installer-*.exe ...
python ohrpackage.py win full+vikings distrib\ohrrpgce-win-installer-{TODAY}-{BRANCH}.exe --iscc "%ISCC%" || exit /b 1

ECHO ------------------------------------------
ECHO Packaging debug info archive
python ohrpackage.py win symbols distrib\ohrrpgce-symbols-win-%BUILDNAME%-{REV}-{TODAY}-{BRANCH}.7z || exit /b 1

ECHO ------------------------------------------
ECHO Packaging source snapshot zip ...
python ohrpackage.py win source distrib\ohrrpgce-source-{TODAY}-{BRANCH}.zip || exit /b 1

ECHO Done.
