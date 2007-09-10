@echo off
echo Now compiling GAME with %1 graphics module, and %2 music module
fbc -lang deprecated verprint.bas
verprint %1 %2
fbc -lang deprecated -g -s gui -mt -m game game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas gfx_%1.bas music_%2.bas loading.bas common.bas browse.bas util.bas gicon.rc -d IS_GAME  %3 %4 %5 %6 %7
echo.
