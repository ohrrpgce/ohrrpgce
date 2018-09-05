@echo off
REM This file is shared between distrib.bat and distrib-nightly-win.bat

set SCONS_ARGS= release=1

set SCPHOST=james_paige@motherhamster.org
set SCPDEST=HamsterRepublic.com/ohrrpgce/nightly
set SCPDOCS=HamsterRepublic.com/ohrrpgce/nightly/docs

ECHO Searching for support programs...

REM Find iscc
SET ISCC="C:\Program Files\Inno Setup 5\iscc.exe"
REM In case we need the 32 bit versions on a 64 bit system...
IF NOT EXIST %ISCC% SET ISCC="C:\Program Files (x86)\Inno Setup 5\iscc.exe"

REM Find svn
REM This checks whether svn is in PATH, otherwise does SVN=
for %%X in (svn.exe) do set SVN=%%~$PATH:X
IF NOT EXIST "%SVN%" (
    REM Nope, check default locations

    SET SVN="C:\Program Files\Subversion\bin\svn.exe"
    IF NOT EXIST "%SVN%" SET SVN="C:\Program Files (x86)\Subversion\bin\svn.exe"

    REM Also support the Sliksvn install location
    IF NOT EXIST "%SVN%" SET SVN="C:\Program Files\Sliksvn\bin\svn.exe"
    IF NOT EXIST "%SVN%" SET SVN="C:\Program Files (x86)\Sliksvn\bin\svn.exe"

    REM Don't throw an error if couldn't find svn, since only distrib.bat needs it
)

for %%X in (cp.exe zip.exe grep.exe sed.exe) do (
    if not exist "support\%%X" (
        ECHO "ERROR: Support file %%X is missing. Unable to continue."
        exit /b 1
    )
)

IF NOT EXIST %ISCC% (
    ECHO "ERROR: Innosetup 5 is missing, unable to continue."
    ECHO "Default location: %ISCC%"
    ECHO "Download from http://www.jrsoftware.org/isdl.php"
    exit /b 1
)

for %%X in (euc.exe) do set EUC=%%~$PATH:X
IF NOT EXIST "%EUC%" (
    ECHO "ERROR: Euphoria is missing (not in the PATH). Unable to continue."
    ECHO "Download from http://www.OpenEuphoria.com/"
    exit /b 1
)

exit /b 0
