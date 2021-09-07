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
REM lto=1 to reduce unlump/relump size
CALL scons unlump relump %SCONS_ARGS% lto=1 || exit /b 1

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

REM ------------------------------------------
ECHO Erasing old distrib files ...
support\rm -f distrib\ohrrpgce-player-win-minimal-sdl2.zip
support\rm -f distrib\ohrrpgce-minimal.zip
support\rm -f distrib\ohrrpgce.zip
support\rm -f distrib\ohrrpgce-win-installer.exe

REM ------------------------------------------
ECHO Generating buildinfo.ini
support\rm -f buildinfo.ini
game.exe -buildinfo buildinfo.ini
ECHO Packaging game player ohrrpgce-player-win-minimal-sdl2.zip ...
support\zip -9 -q distrib\ohrrpgce-player-win-minimal-sdl2.zip game.exe buildinfo.ini SDL2.dll SDL2_mixer.dll gfx_directx.dll LICENSE-binary.txt README-player-only.txt

REM ------------------------------------------
ECHO Packaging minimal-but-complete ohrrpgce-minimal.zip ...
REM Note: linux and mac "minimal" .tar.gz files contain only the player, while
REM windows "minimal" .zip files contain nearly everything except vikings and import.
support\cp game.exe tmpdist
support\cp custom.exe tmpdist
support\cp hspeak.exe tmpdist
support\cp plotscr.hsd tmpdist
support\cp scancode.hsi tmpdist
support\cp README-game.txt tmpdist
support\cp README-custom.txt tmpdist
support\cp IMPORTANT-nightly.txt tmpdist
support\cp LICENSE-binary.txt tmpdist
support\cp whatsnew.txt tmpdist
support\cp SDL2.dll tmpdist
support\cp SDL2_mixer.dll tmpdist
support\cp gfx_directx.dll tmpdist
REM wget.exe needed to download everything else (unzip is downloaded as an .exe)
mkdir tmpdist\support
support\cp support\wget.exe tmpdist\support
support\cp -r data tmpdist\data
support\cp -r ohrhelp tmpdist\ohrhelp
mkdir tmpdist\docs
support\cp docs\*.URL tmpdist\docs
support\cp docs\*.png tmpdist\docs
support\cp docs\plotdictionary.html tmpdist\docs
support\cp docs\more-docs.txt tmpdist\docs

cd tmpdist
..\support\zip -9 -q -r ..\distrib\ohrrpgce-minimal.zip *.*
cd ..

rmdir /s /q tmpdist
mkdir tmpdist
cd tmpdist
..\support\unzip -q ..\distrib\ohrrpgce-minimal.zip game.exe
cd ..
IF NOT EXIST tmpdist\game.exe GOTO SANITYFAIL
del tmpdist\game.exe

REM ------------------------------------------
ECHO Packaging ohrrpgce.zip ...
rmdir /s /q tmpdist
mkdir tmpdist
support\cp game.exe tmpdist
support\cp custom.exe tmpdist
support\cp hspeak.exe tmpdist
support\cp README-game.txt tmpdist
support\cp README-custom.txt tmpdist
support\cp IMPORTANT-nightly.txt tmpdist
support\cp whatsnew.txt tmpdist
support\cp LICENSE.txt tmpdist
support\cp LICENSE-binary.txt tmpdist
support\cp plotscr.hsd tmpdist
support\cp scancode.hsi tmpdist
support\cp gfx_directx.dll tmpdist
support\cp SDL2.dll tmpdist
support\cp SDL2_mixer.dll tmpdist
support\cp -r data tmpdist\data
support\cp -r ohrhelp tmpdist\ohrhelp
mkdir tmpdist\docs
support\cp docs\FAQ.URL tmpdist\docs
support\cp docs\HOWTO.URL tmpdist\docs
support\cp docs\*.html tmpdist\docs
support\cp docs\*.png tmpdist\docs
support\cp docs\plotdict.xml tmpdist\docs
support\cp docs\htmlplot.xsl tmpdist\docs
support\cp docs\more-docs.txt tmpdist\docs
mkdir tmpdist\support
support\cp support\madplay.exe tmpdist\support
support\cp support\LICENSE-madplay.txt tmpdist\support
support\cp support\oggenc.exe tmpdist\support
support\cp support\LICENSE-oggenc.txt tmpdist\support
support\cp support\wget.exe tmpdist\support
support\cp support\wget.hlp tmpdist\support
support\cp support\zip.exe tmpdist\support
support\cp support\unzip.exe tmpdist\support
support\cp support\CrashRpt*.dll support\CrashSender*.exe support\crashrpt_lang.ini tmpdist\support
support\cp support\LICENSE-crashrpt.txt tmpdist\support
support\cp support\rcedit.exe tmpdist\support
support\cp support\LICENSE-rcedit.txt tmpdist\support
support\cp support\*-version.txt tmpdist\support
support\cp relump.exe tmpdist\support
support\cp unlump.exe tmpdist\support
support\cp -r import tmpdist\import

cd tmpdist
..\support\zip -9 -q -r ..\distrib\ohrrpgce.zip *.* -x *.svn*
cd ..

ECHO Sanity checking ohrrpgce.zip
rmdir /s /q tmpdist
mkdir tmpdist
cd tmpdist
..\support\unzip -q ..\distrib\ohrrpgce.zip custom.exe
cd ..
IF NOT EXIST tmpdist\custom.exe GOTO SANITYFAIL
del tmpdist\custom.exe

REM ------------------------------------------
ECHO Packaging ohrrpgce-win-installer.exe ...
echo. > iextratxt.txt
"%ISCC%" /Q /Odistrib /Fohrrpgce-win-installer ohrrpgce.iss
del iextratxt.txt
IF NOT EXIST distrib\ohrrpgce-win-installer.exe (
    ECHO Inno Setup failed!
    exit /b 1
)

REM ------------------------------------------
ECHO Packaging debug info archive
support\rm -f distrib/ohrrpgce-symbols-win.7z
REM The ./ path prefixes add the files with the win32/ relative path
support\7za a -mx=7 -bd distrib\ohrrpgce-symbols-win.7z game.exe custom.exe .\win32\custom.pdb .\win32\game.pdb > NUL
REM NOTE: 7za will produce a .7z file even if input files are missing. The -sse arg to stop that doesn't work!
IF NOT EXIST distrib\ohrrpgce-symbols-win.7z (
    ECHO 7za failed!
    exit /b 1
)

REM ------------------------------------------
ECHO Packaging source snapshot zip ...
IF NOT EXIST "%SVN%" (
    ECHO "ERROR: SVN (Subversion) is neither in PATH or in a default location, unable to continue."
    ECHO "Download from https://subversion.apache.org/"
    exit /b 1
)
CALL distver.bat
RMDIR /s /q tmpdist
MKDIR tmpdist
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

REM ------------------------------------------
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
