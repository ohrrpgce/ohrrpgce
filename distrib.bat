@ECHO OFF

SET ISCC="C:\Program Files\Inno Setup 5\iscc.exe"
SET SVN="C:\Program Files\Subversion\bin\svn.exe"

ECHO Verifying support programs...
IF NOT EXIST support\cp.exe GOTO NOSUPPORT
IF NOT EXIST support\zip.exe GOTO NOSUPPORT
IF NOT EXIST %ISCC% GOTO NOINNO
IF NOT EXIST %EUDIR%\bin\exw.exe GOTO NOEUPHORIA

IF NOT EXIST tmpdist GOTO SKIPDELTMPDIST
RMDIR /S /Q tmpdist
:SKIPDELTMPDIST
MKDIR tmpdist

REM ------------------------------------------
ECHO Building executables...

del game.exe custom.exe relump.exe unlump.exe hspeak.exe

ECHO   Windows executables...
CALL make.bat > NUL
IF NOT EXIST game.exe GOTO NOEXE
IF NOT EXIST custom.exe GOTO NOEXE

ECHO   Utilities...
CALL makeutil.bat > NUL
IF NOT EXIST unlump.exe GOTO NOEXE
IF NOT EXIST relump.exe GOTO NOEXE

ECHO   Hspeak compiler...
CALL makehspeak.bat > NUL
IF NOT EXIST hspeak.exe GOTO NOEXE

ECHO   Lumping Vikings of Midgard
IF NOT EXIST vikings.rpg GOTO SKIPDELVIKING
DEL vikings.rpg
:SKIPDELVIKING
relump ..\games\vikings\vikings.rpgdir vikings.rpg > NUL
IF NOT EXIST vikings.rpg GOTO NORPG

CALL update-import.bat 

REM ------------------------------------------
ECHO Erasing old distrib files ...

IF NOT EXIST distrib\ohrrpgce-floppy.zip GOTO DONEDELFLOPPY
del distrib\ohrrpgce-floppy.zip
:DONEDELFLOPPY
IF NOT EXIST distrib\ohrrpgce.zip GOTO DONEDELCUSTOM
del distrib\ohrrpgce.zip
:DONEDELCUSTOM
IF NOT EXIST distrib\ohrrpgce-win-installer.exe GOTO DONEDELINSTALL
del distrib\ohrrpgce-win-installer.exe
:DONEDELINSTALL

REM ------------------------------------------
ECHO Packaging minimalist ohrrpgce-floppy.zip ...
del tmpdist\*.???
support\cp game.exe tmpdist
support\cp custom.exe tmpdist
support\cp hspeak.exe tmpdist
support\cp ohrrpgce.new tmpdist
support\cp plotscr.hsd tmpdist
support\cp scancode.hsi tmpdist
support\cp README-game.txt tmpdist
support\cp README-custom.txt tmpdist
support\cp LICENSE-binary.txt tmpdist
support\cp SDL.dll tmpdist
support\cp SDL_mixer.dll tmpdist
support\cp support\madplay+oggenc.URL tmpdist
mkdir tmpdist\docs
support\cp docs\*.URL tmpdist\docs

cd tmpdist
..\support\zip -9 -q -r ..\distrib\ohrrpgce-floppy.zip *.*
cd ..

del tmpdist\*.???
RMDIR /s /q tmpdist\docs
cd tmpdist
..\support\unzip -q ..\distrib\ohrrpgce-floppy.zip game.exe
cd ..
IF NOT EXIST tmpdist\game.exe GOTO SANITYFAIL
del tmpdist\game.exe

REM ------------------------------------------
ECHO Packaging ohrrpgce.zip ...
del tmpdist\*.???
support\cp game.exe tmpdist
support\cp custom.exe tmpdist
support\cp hspeak.exe tmpdist
support\cp ohrrpgce.new tmpdist
support\cp README-custom.txt tmpdist
support\cp LICENSE.txt tmpdist
support\cp LICENSE-binary.txt tmpdist
support\cp plotscr.hsd tmpdist
support\cp scancode.hsi tmpdist
support\cp SDL.dll tmpdist
support\cp SDL_mixer.dll tmpdist
mkdir tmpdist\docs
support\cp docs\FAQ.URL tmpdist\docs
support\cp docs\HOWTO.URL tmpdist\docs
support\cp docs\*.html tmpdist\docs
support\cp docs\plotdict.xml tmpdist\docs
support\cp docs\htmlplot.xsl tmpdist\docs
support\cp docs\more-docs.txt tmpdist\docs
mkdir tmpdist\support
support\cp support\madplay.exe tmpdist\support
support\cp support\LICENSE-madplay.txt tmpdist\support
support\cp support\oggenc.exe tmpdist\support
support\cp support\LICENSE-oggenc.txt tmpdist\support
support\cp vikings.rpg tmpdist
support\cp ..\games\vikings\vikings.hss tmpdist
support\cp ..\games\vikings\vikings.hsi tmpdist
support\cp ..\games\vikings\utility.hsi tmpdist
support\cp ..\games\vikings\README-vikings.txt tmpdist

mkdir tmpdist\import
mkdir tmpdist\import\background
support\cp import\background\*.bmp tmpdist\import\background
mkdir tmpdist\import\fonts
support\cp import\fonts\*.ohf tmpdist\import\fonts
mkdir tmpdist\import\Music
support\cp import\Music\*.* tmpdist\import\Music
mkdir "tmpdist\import\Sound Effects"
support\cp import/"Sound Effects"/*.ogg tmpdist/import/"Sound Effects"

cd tmpdist
..\support\zip -9 -q -r ..\distrib\ohrrpgce.zip *.* -x *.svn*
cd ..

del tmpdist\*.???
cd tmpdist
..\support\unzip -q ..\distrib\ohrrpgce.zip custom.exe
cd ..
IF NOT EXIST tmpdist\custom.exe GOTO SANITYFAIL
del tmpdist\custom.exe

REM ------------------------------------------
ECHO Packaging ohrrpgce-win-installer.exe ...
%ISCC% /Q /Odistrib /Fohrrpgce-win-installer ohrrpgce.iss
IF NOT EXIST distrib\ohrrpgce-win-installer.exe GOTO SANITYFAIL

REM ------------------------------------------
ECHO Packaging source snapshot zip ...
IF NOT EXIST %SVN% GOTO NOSVN
IF NOT EXIST support\grep.exe GOTO NOSUPPORT
IF NOT EXIST support\sed.exe GOTO NOSUPPORT
CALL distver.bat
DEL tmpdist\*.???
RMDIR /s /q tmpdist\support
RMDIR /s /q tmpdist\import
RMDIR /s /q tmpdist\docs
CD tmpdist
%SVN% info .. | ..\support\grep "^URL:" | ..\support\sed s/"^URL: "/"SET REPOSITORY="/ > svnrepo.bat
CALL svnrepo.bat
ECHO   Checkout...
%SVN% co -q %REPOSITORY%
del svnrepo.bat
ECHO   Zip...
..\support\zip -q -r ..\distrib\ohrrpgce-source.zip *.*
cd ..

REM ------------------------------------------
ECHO Cleaning up...
rmdir /s /q tmpdist

REM ------------------------------------------
ECHO Rename results...
ECHO %OHRVERDATE%-%OHRVERCODE%
move distrib\ohrrpgce-floppy.zip distrib\ohrrpgce-floppy-%OHRVERDATE%-%OHRVERCODE%.zip
move distrib\ohrrpgce.zip distrib\ohrrpgce-%OHRVERDATE%-%OHRVERCODE%.zip
move distrib\ohrrpgce-win-installer.exe distrib\ohrrpgce-win-installer-%OHRVERDATE%-%OHRVERCODE%.exe
move distrib\ohrrpgce-source.zip distrib\ohrrpgce-source-%OHRVERDATE%-%OHRVERCODE%.zip

REM ------------------------------------------
ECHO Done.
GOTO DONE

REM ------------------------------------------
:NOSUPPORT
ECHO ERROR: Support files are missing, unable to continue.
GOTO DONE

:NOINNO
ECHO ERROR: Innosetup 5 is missing, unable to continue.
ECHO Default location: %ISCC%
ECHO Download from http://www.jrsoftware.org/isdl.php
GOTO DONE

:NOSVN
ECHO ERROR: SVN (Subversion) is missing, unable to continue.
ECHO Default location: %SVN%
ECHO Download from http://subversion.tigris.org/
GOTO DONE

:NOEUPHORIA
ECHO ERROR: Euphoria is missing, unable to continue.
ECHO Default location: %EUDIR%
ECHO Download from http://www.RapidEuphoria.com/
GOTO DONE

:NOEXE
ECHO ERROR: An executable failed to build, unable to continue.
GOTO DONE

:NORPG
ECHO ERROR: Failed to relump vikings of midgard
GOTO DONE

:SANITYFAIL
ECHO ERROR: Sanity test failed, distribution files are incomplete!
GOTO DONE

REM ------------------------------------------
:DONE