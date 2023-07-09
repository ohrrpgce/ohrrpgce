CALL distrib-win-setup.bat || exit /b 1

ECHO ==========================================
ECHO Building sdl2 executables...

REM scons continues if can't create the pdb files
support\rm -f game.exe custom.exe relump.exe unlump.exe hspeak.exe win32\game.pdb win32\custom.pdb

REM Equivalent to gfx=sdl2+directx+fb music=sdl2
CALL scons game custom buildname=sdl2 %SCONS_ARGS% || exit /b 1
REM Would compile with lto=1 to reduce unlump/relump size, but that causes mingw-w64 gcc 8.1.0 to crash
CALL scons hspeak unlump relump win95=1 sse2=0 %SCONS_ARGS% || exit /b 1

ECHO ------------------------------------------
ECHO Packaging game player ohrrpgce-player-win-*-sdl2.zip ...
python ohrpackage.py win player distrib\ohrrpgce-player-win-{DATE}-{BRANCH}-sdl2.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging minimal-but-complete ohrrpgce-win-minimal-*.zip ...
python ohrpackage.py win minimal distrib\ohrrpgce-win-{DATE}-{BRANCH}-minimal.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging full ohrrpgce-win-*.zip ...
python ohrpackage.py win full distrib\ohrrpgce-win-{DATE}-{BRANCH}.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging ohrrpgce-win-installer-*.exe ...
python ohrpackage.py win full+vikings distrib\ohrrpgce-win-installer-{DATE}-{BRANCH}.exe --iscc "%ISCC%" || exit /b 1

ECHO ------------------------------------------
ECHO Packaging sdl2 debug info archive
python ohrpackage.py win symbols distrib\ohrrpgce-symbols-win-{BUILDNAME}-{REV}-{DATE}-{BRANCH}.7z || exit /b 1

ECHO ==========================================
ECHO Building win95 executables...

support\rm -f game.exe custom.exe win32\game.pdb win32\custom.pdb
REM win95=1 implies gfx=directx+sdl+fb music=sdl
CALL scons game custom win95=1 sse2=0 buildname=win95 %SCONS_ARGS% || exit /b 1

ECHO ------------------------------------------
ECHO Packaging game player ohrrpgce-player-win-*-win95.zip ...
python ohrpackage.py win player distrib\ohrrpgce-player-win-{DATE}-{BRANCH}-win95.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging full ohrrpgce-win-*-win95.zip ...
python ohrpackage.py win full distrib\ohrrpgce-win-{DATE}-{BRANCH}-win95.zip || exit /b 1

ECHO ------------------------------------------
ECHO Packaging win95 debug info archive
python ohrpackage.py win symbols distrib\ohrrpgce-symbols-win-{BUILDNAME}-{REV}-{DATE}-{BRANCH}.7z || exit /b 1

ECHO ==========================================
ECHO Packaging source snapshot zip ...
python ohrpackage.py win source distrib\ohrrpgce-source-{DATE}-{BRANCH}.zip || exit /b 1

ECHO Done.
