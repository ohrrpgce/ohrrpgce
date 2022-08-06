CALL distrib-win-setup.bat || exit /b 1

ECHO ==========================================
ECHO Building sdl2 executables...

REM This should be the same BUILDNAME (as in distrib-nightly-win-packnupload.bat) as the default nightly
set BUILDNAME=sdl2

REM scons continues if can't create the pdb files
support\rm -f game.exe custom.exe relump.exe unlump.exe hspeak.exe win32\game.pdb win32\custom.pdb

REM Equivalent to gfx=sdl2+directx+fb music=sdl2
CALL scons game custom %SCONS_ARGS% || exit /b 1
REM Would compile with lto=1 to reduce unlump/relump size, but that causes mingw-w64 gcc 8.1.0 to crash
CALL scons hspeak unlump relump win95=1 sse2=0 %SCONS_ARGS% || exit /b 1

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
ECHO Packaging sdl2 debug info archive
python ohrpackage.py win symbols distrib\ohrrpgce-symbols-win-%BUILDNAME%-{REV}-{TODAY}-{BRANCH}.7z || exit /b 1

ECHO ==========================================
ECHO Building win95 executables...

REM This should be the same BUILDNAME (as in distrib-nightly-win-packnupload.bat) as ohrrpgce-win-win95-wip.zip
set BUILDNAME=music_sdl

support\rm -f game.exe custom.exe win32\game.pdb win32\custom.pdb
REM Equivalent to gfx=directx+sdl+fb music=sdl
CALL scons game custom win95=1 sse2=0 %SCONS_ARGS% || exit /b 1

ECHO ------------------------------------------
ECHO Packaging game player ohrrpgce-player-win-win95-*.zip ...
python ohrpackage.py win player distrib\ohrrpgce-player-win-win95-{TODAY}-{BRANCH}.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging ohrrpgce-win95-*.zip ...
python ohrpackage.py win full distrib\ohrrpgce-win95-{TODAY}-{BRANCH}.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging win95 debug info archive
python ohrpackage.py win symbols distrib\ohrrpgce-symbols-win-%BUILDNAME%-{REV}-{TODAY}-{BRANCH}.7z || exit /b 1

ECHO ==========================================
ECHO Packaging source snapshot zip ...
python ohrpackage.py win source distrib\ohrrpgce-source-{TODAY}-{BRANCH}.zip || exit /b 1

ECHO Done.
