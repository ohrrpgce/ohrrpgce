@echo off
echo See notes in allegro-static.txt
pause
@echo on
copy /y codename-al.txt codename.txt
verprint
fbc -x customa.exe -s gui -d ALLEGRO_STATICLINK -m custom custom.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas bam2mid.bas gfx_alleg.bas music_allegro.bas cicon.rc

