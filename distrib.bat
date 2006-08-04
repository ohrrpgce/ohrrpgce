@ECHO OFF

SET ISCC="C:\Program Files\Inno Setup 5\iscc.exe"
SET SVN="C:\Program Files\Subversion\bin\svn.exe"

ECHO Verifying support programs...
IF NOT EXIST support\cp.exe GOTO NOSUPPORT
IF NOT EXIST support\zip.exe GOTO NOSUPPORT
IF NOT EXIST %ISCC% GOTO NOINNO

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

REM ------------------------------------------
ECHO Erasing old distrib files ...

IF NOT EXIST distrib\ohrrpgce.zip GOTO DONEDELCUSTOM
del distrib\ohrrpgce.zip
:DONEDELCUSTOM
IF NOT EXIST distrib\ohrrpgce-win-installer.exe GOTO DONEDELINSTALL
del distrib\ohrrpgce-win-installer.exe
:DONEDELINSTALL

REM ------------------------------------------
ECHO Packaging minimalist ohrrpgce.zip ...
del tmpdist\*.???
support\cp game.exe tmpdist
support\cp custom.exe tmpdist
support\cp hspeak.exe tmpdist
support\cp ohrrpgce.mas tmpdist
support\cp ohrrpgce.fnt tmpdist
support\cp ohrrpgce.new tmpdist
support\cp README-custom.txt tmpdist
support\cp LICENSE.txt tmpdist
support\cp LICENSE-binary.txt tmpdist
support\cp plotscr.hsd tmpdist
support\cp scancode.hsi tmpdist
support\cp docs\more-docs.txt tmpdist

cd tmpdist
..\support\zip -q -r ..\distrib\ohrrpgce.zip *.*
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
del tmpdist\*.???
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

:NOEXE
ECHO ERROR: An executable failed to build, unable to continue.
GOTO DONE

:SANITYFAIL
ECHO ERROR: Sanity test failed, distribution files are incomplete!
GOTO DONE

REM ------------------------------------------
:DONE