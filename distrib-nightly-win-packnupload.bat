CALL distver.bat
REM BUILDNAME must be the same as the buildname arg to scons
set BUILDNAME=%1
set ZIPFILE=ohrrpgce-win-%BUILDNAME%-%OHRVERCODE%.zip
set SYMBFILE=ohrrpgce-symbols-win-%BUILDNAME%-r%SVNREV%-%OHRVERDATE%-%OHRVERCODE%.7z
echo     Packaging %BUILDNAME% nightly

python ohrpackage.py win nightly distrib\%ZIPFILE% -- %2 %3 %4 %5 %6 %7 %8 %9 && (

  echo     Packaging %BUILDNAME% symbols
  python ohrpackage.py win symbols distrib\%SYMBFILE%
  IF ERRORLEVEL 1 (
    ECHO Skipping %BUILDNAME% nightly because missing symbols!
  ) ELSE (
    pscp -q distrib\%SYMBFILE% %SCPHOST%:%SCPSYMBOLS%
    pscp -q distrib\%ZIPFILE% %SCPHOST%:%SCPDEST%
  )
)

echo.
echo.
