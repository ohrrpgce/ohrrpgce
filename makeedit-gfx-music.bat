@echo off
echo Now compiling CUSTOM with %1 graphics module, and %2 music module
fbc verprint.bas
verprint %1 %2
fbc -s gui -m custom custom.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas bam2mid.bas gfx_%1.bas music_%2.bas loading.bas common.bas util.bas cicon.rc -d IS_CUSTOM  %3 %4 %5 %6 %7
echo.
