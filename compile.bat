@echo off
verprint
verprint > compile.out
call callbc.bat game
call callbc.bat bmod
call callbc.bat bmodsubs
call callbc.bat menustuf
call callbc.bat moresubs
call callbc.bat yetmore
call callbc.bat custom
call callbc.bat drawing
call callbc.bat subs
call callbc.bat subs2
call callbc.bat subs3
call callbc.bat mapsubs
call callbc.bat flexmenu
support\pkunzip.exe -o support\nocom.zip nocom.obj > NUL
support\pkunzip.exe -o support\freelink.zip freelink.exe > NUL
freelink.exe @game.%LINKEXT%
freelink.exe @custom.%LINKEXT%
del *.obj
del freelink.exe
grep "\^" compile.out
echo.
