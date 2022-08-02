CALL distrib-win-setup.bat || exit /b 1

IF EXIST tmpdist RMDIR /S /Q tmpdist
MKDIR tmpdist

REM ------------------------------------------
ECHO Building executables...

REM This should be the BUILDNAME (as in distrib-nightly-win-packnupload.bat) for the default backends
set BUILDNAME=sdl2

support\rm -f game.exe custom.exe relump.exe unlump.exe hspeak.exe

ECHO   Windows executables...
CALL scons game custom hspeak %SCONS_ARGS% || exit /b 1
REM Would compile with lto=1 to reduce unlump/relump size, but that causes mingw-w64 gcc 8.1.0 to crash
CALL scons unlump relump %SCONS_ARGS% || exit /b 1

REM "scons pdb=1" will continue even if it can't produce these
FOR %%X IN (win32\game.pdb win32\custom.pdb) DO (
    IF NOT EXIST "%%X" (
        ECHO "ERROR: build result %%X is missing. Unable to continue."
        GOTO SANITYFAIL
    )
)

ECHO   Lumping Vikings of Midgard
support\rm -f vikings.rpg
relump vikings\vikings.rpgdir vikings.rpg > NUL
IF NOT EXIST vikings.rpg (
    ECHO "ERROR: Failed to relump vikings of midgard"
    exit /b 1
)

ECHO ------------------------------------------
ECHO Erasing old distrib files ...
support\rm -f distrib\ohrrpgce-player-win-minimal-sdl2.zip
support\rm -f distrib\ohrrpgce-minimal.zip
support\rm -f distrib\ohrrpgce.zip
support\rm -f distrib\ohrrpgce-win-installer.exe
support\rm -f distrib\ohrrpgce-symbols-win.7z

ECHO ------------------------------------------
ECHO Packaging game player ohrrpgce-player-win-minimal-sdl2.zip ...
python ohrpackage.py win player distrib\ohrrpgce-player-win-minimal-sdl2.zip

ECHO ------------------------------------------
ECHO Packaging minimal-but-complete ohrrpgce-minimal.zip ...
REM Note: linux and mac "minimal" .tar.gz files contain only the player, while
REM Windows "minimal" .zip files contain nearly everything except support, import and Vikings
python ohrpackage.py win minimal distrib\ohrrpgce-minimal.zip

ECHO ------------------------------------------
ECHO Packaging ohrrpgce.zip ...
python ohrpackage.py win full distrib\ohrrpgce.zip

ECHO ------------------------------------------
ECHO Packaging ohrrpgce-win-installer.exe ...
python ohrpackage.py win full+vikings distrib\ohrrpgce-win-installer.exe
IF NOT EXIST distrib\ohrrpgce-win-installer.exe (
    ECHO Inno Setup failed!
    exit /b 1
)

ECHO ------------------------------------------
ECHO Packaging debug info archive
python ohrpackage.py win symbols distrib\ohrrpgce-symbols-win.7z
IF NOT EXIST distrib\ohrrpgce-symbols-win.7z (
    ECHO 7za failed!
    exit /b 1
)

ECHO ------------------------------------------
ECHO Packaging source snapshot zip ...
IF NOT EXIST "%SVN%" (
    ECHO "ERROR: SVN (Subversion) is neither in PATH or in a default location, unable to continue."
    ECHO "Download from https://subversion.apache.org/"
    exit /b 1
)
CALL distver.bat
CD tmpdist
"%SVN%" info .. | ..\support\grep "^URL:" | ..\support\sed s/"^URL: "/"SET REPOSITORY="/ > svnrepo.bat
CALL svnrepo.bat
ECHO   Checkout...
"%SVN%" co -q %REPOSITORY%
"%SVN%" info %OHRVERBRANCH% > %OHRVERBRANCH%/svninfo.txt
del svnrepo.bat
ECHO   Zip...
..\support\zip -q -r ..\distrib\ohrrpgce-source.zip *.* -x "*\vikings\*"
cd ..

ECHO ------------------------------------------
ECHO Cleaning up...
rmdir /s /q tmpdist

REM ------------------------------------------
ECHO Rename results...
ECHO %OHRVERDATE%-%OHRVERBRANCH%
move distrib\ohrrpgce-player-win-minimal-sdl2.zip distrib\ohrrpgce-player-win-minimal-sdl2-%OHRVERDATE%-%OHRVERBRANCH%.zip
move distrib\ohrrpgce-minimal.zip distrib\ohrrpgce-minimal-%OHRVERDATE%-%OHRVERBRANCH%.zip
move distrib\ohrrpgce.zip distrib\ohrrpgce-%OHRVERDATE%-%OHRVERBRANCH%.zip
move distrib\ohrrpgce-win-installer.exe distrib\ohrrpgce-win-installer-%OHRVERDATE%-%OHRVERBRANCH%.exe
move distrib\ohrrpgce-source.zip distrib\ohrrpgce-source-%OHRVERDATE%-%OHRVERBRANCH%.zip
move distrib\ohrrpgce-symbols-win.7z distrib\ohrrpgce-symbols-win-%BUILDNAME%-r%SVNREV%-%OHRVERDATE%-%OHRVERBRANCH%.7z

REM ------------------------------------------
ECHO Done.
GOTO DONE

REM ------------------------------------------

:SANITYFAIL
ECHO ERROR: Sanity test failed, distribution files are incomplete!
exit /b 1

REM ------------------------------------------
:DONE
