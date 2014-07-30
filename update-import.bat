ECHO   Downloading import.zip
IF NOT EXIST import.zip GOTO SKIPDELIMPORT
DEL import.zip
:SKIPDELIMPORT
wget -q http://rpg.hamsterrepublic.com/ohrimport/import.zip
IF NOT EXIST import.zip GOTO NOIMPORT

DEL /Q import\Music\*.* > NUL
DEL /Q "import\Sound Effects"\*.* > NUL
ECHO   Unpacking import.zip
support\unzip -q -d import\ import.zip

GOTO DONE

:NOIMPORT
ECHO ERROR: Unable to download http://rpg.hamsterrepublic.com/ohrimport/import.zip
ECHO please download it manually and leave it in the same folder as this batch file
GOTO DONE

:DONE
