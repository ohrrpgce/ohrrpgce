@echo off
echo See notes in allegro-static.txt
pause
@echo on
copy /y codename-al.txt codename.txt
verprint
fbc -x gamea.exe -d ALLEGRO_STATICLINK -m game -s gui game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas gfx_alleg.bas music_allegro.bas gicon.rc

