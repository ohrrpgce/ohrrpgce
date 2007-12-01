@echo off
echo Now compiling CUSTOM with %OHRGFX% graphics module, and %OHRMUSIC% music module
fbc -lang deprecated verprint.bas
verprint %OHRGFX% %OHRMUSIC%
fbc -lang deprecated -s gui -m custom custom.bas customsubs.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas bam2mid.bas gfx_%OHRGFX%.bas music_%OHRMUSIC%.bas loading.bas common.bas browse.bas util.bas cicon.rc -d IS_CUSTOM  %1 %2 %3 %4 %5
echo.
