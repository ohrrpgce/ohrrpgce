@echo off
DEL compat.bi
DEL compat.bas
copy qbcompat.bi compat.bi
copy qbcompat.bas compat.bas
verprint
verprint > compile.out
IF '%1%'=='c' GOTO nogame
call callbc.bat game
call callbc.bat bmod
call callbc.bat bmodsubs
call callbc.bat menustuf
call callbc.bat moresubs
call callbc.bat yetmore
call callbc.bat yetmore2
:nogame
call callbc.bat compat
IF '%1%'=='g' GOTO nocust
call callbc.bat custom
call callbc.bat drawing
call callbc.bat subs
call callbc.bat subs2
call callbc.bat subs3
call callbc.bat mapsubs
call callbc.bat flexmenu
call callbc.bat menus
:nocust
call callbc.bat loading
support\pkunzip.exe -o support\nocom.zip nocom.obj > NUL
support\pkunzip.exe -o support\freelink.zip freelink.exe > NUL
IF '%1%'=='c' GOTO nogame2
freelink.exe @game.%LINKEXT%
:nogame2
IF '%1%'=='g' GOTO nocust2
freelink.exe @custom.%LINKEXT%
:nocust2
del *.obj
del freelink.exe
grep "\^" compile.out
echo.
