@echo off

ECHO This batch file no longer works properly unless you,
ECHO have obscene amounts of free conventional DOS memory

echo debug build
echo debug build > compile.out

support\pkzip.exe debugbas.zip *.bas game.exe custom.exe > NUL
IF NOT EXIST debugbas.zip GOTO NOPKZIP

verprint.exe
verprint >> compile.out

cat gver.txt|sed s/"O.H.R.RPG.C.E version"/"OHRRPGCE debug build"/ > gver.txt
cat cver.txt|sed s/"OHRRPGCE Editor:"/"OHRRPGCE Debug:"/ > cver.txt

cat game.bas | sed s/"'DEBUG debug"/"debug"/ > game.bas
call callbc.bat game
cat bmod.bas | sed s/"'DEBUG debug"/"debug"/ > bmod.bas
call callbc.bat bmod
cat bmodsubs.bas | sed s/"'DEBUG debug"/"debug"/ > bmodsubs.bas
call callbc.bat bmodsubs
cat menustuf.bas | sed s/"'DEBUG debug"/"debug"/ > menustuf.bas
call callbc.bat menustuf
cat moresubs.bas | sed s/"'DEBUG debug"/"debug"/ > moresubs.bas
call callbc.bat moresubs
cat yetmore.bas | sed s/"'DEBUG debug"/"debug"/ > yetmore.bas
call callbc.bat yetmore
cat custom.bas | sed s/"'DEBUG debug"/"debug"/ > custom.bas
call callbc.bat custom
cat drawing.bas | sed s/"'DEBUG debug"/"debug"/ > drawing.bas
call callbc.bat drawing
cat mapsubs.bas | sed s/"'DEBUG debug"/"debug"/ > mapsubs.bas
call callbc.bat mapsubs
cat subs.bas | sed s/"'DEBUG debug"/"debug"/ > subs.bas
call callbc.bat subs
cat subs2.bas | sed s/"'DEBUG debug"/"debug"/ > subs2.bas
call callbc.bat subs2
cat ironhoof.bas | sed s/"'DEBUG debug"/"debug"/ > ironhoof.bas
call callbc.bat ironhoof
cat flexmenu.bas | sed s/"'DEBUG debug"/"debug"/ > flexmenu.bas
call callbc.bat flexmenu

%QBDIR%\bc %OHRRPGCE%\flexmenu.bas/O/AH/T/E/C:1;>>compile.out
move GAME.EXE GAME.EX_      > NUL
move CUSTOM.EXE CUSTOM.EX_  > NUL
support\pkunzip.exe -o support\nocom.zip nocom.obj > NUL
support\pkunzip.exe -o support\freelink.zip freelink.exe > NUL
freelink.exe @game.lnk
freelink.exe @custom.lnk
del *.obj
move GAME.EXE G_DEBUG.EXE   > NUL
move CUSTOM.EXE C_DEBUG.EXE > NUL
move GAME.EX_ GAME.EXE      > NUL
move CUSTOM.EX_ CUSTOM.EXE  > NUL
support\pkunzip.exe -o debugbas.zip     > NUL
del debugbas.zip
grep "\^" compile.out
echo.

ECHO This batch file no longer works properly unless you,
ECHO have obscene amounts of free conventional DOS memory

GOTO FINISH

:NOPKZIP
ECHO You must have pkzip.exe installed for this batch file to work.

:FINISH