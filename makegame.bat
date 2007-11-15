@echo off
echo Now compiling GAME with %OHRGFX% graphics module, and %OHRMUSIC% music module
fbc -lang deprecated verprint.bas
verprint %OHRGFX% %OHRMUSIC%
fbc -lang deprecated -s gui -mt -m game game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas gfx_%OHRGFX%.bas music_%OHRMUSIC%.bas loading.bas common.bas browse.bas util.bas gicon.rc -d IS_GAME  %1 %2 %3 %4 %5
echo.
