echo game >compile.out
%QBDIR%\bc \wander\game.bas/O/AH/T/C:512;>>compile.out
echo bmod >>compile.out
%QBDIR%\bc \wander\bmod.bas/O/AH/T/C:512;>>compile.out
echo bmodsubs >>compile.out
%QBDIR%\bc \wander\bmodsubs.bas/O/AH/T/C:512;>>compile.out
echo menustuf >>compile.out
%QBDIR%\bc \wander\menustuf.bas/O/AH/T/C:512;>>compile.out
echo moresubs >>compile.out
%QBDIR%\bc \wander\moresubs.bas/O/AH/T/C:512;>>compile.out
echo yetmore >>compile.out
%QBDIR%\bc \wander\yetmore.bas/O/AH/T/C:512;>>compile.out
echo custom >>compile.out
%QBDIR%\bc \wander\custom.bas/O/AH/T/C:512;>>compile.out
echo drawing >>compile.out
%QBDIR%\bc \wander\drawing.bas/O/AH/T/C:512;>>compile.out
echo subs >>compile.out
%QBDIR%\bc \wander\subs.bas/O/AH/T/C:512;>>compile.out
echo subs2 >>compile.out
%QBDIR%\bc \wander\subs2.bas/O/AH/T/C:512;>>compile.out
echo ironhoof >>compile.out
%QBDIR%\bc \wander\ironhoof.bas/O/AH/T/C:512;>>compile.out
freelink @game.lnk
freelink @custom.lnk
del *.obj
