%QBDIR%\bc %OHRRPGCE%\rpgfix.bas/O/AH/T/C:512; >cfix.out
pkunzip -o support\nocom.zip nocom.obj > NUL
pkunzip -o support\freelink.zip freelink.exe > NUL
freelink @rpgfix.lnk
del *.obj
