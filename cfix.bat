%QBDIR%\bc %OHRRPGCE%\rpgfix.bas/O/AH/T/C:512; >cfix.out
support\pkunzip.exe -o support\nocom.zip nocom.obj > NUL
support\pkunzip.exe -o support\freelink.zip freelink.exe > NUL
freelink @rpgfix.%LINKEXT%
del *.obj
del freelink.exe
