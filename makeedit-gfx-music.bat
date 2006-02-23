copy /y fbcompat.bi compat.bi
copy /y fbcompat.bas compat.bas
verprint
fbc -s gui -m custom custom.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas bam2mid.bas gfx_%1.bas music_%2.bas cicon.rc -d IS_CUSTOM  %3 %4 %5 %6 %7

