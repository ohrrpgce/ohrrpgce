copy /y fbcompat.bi compat.bi
copy /y fbcompat.bas compat.bas
verprint
fbc -s gui -m game game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas gfx_fb2.bas music_mike.bas gicon.rc -d IS_GAME  %1 %2 %3 %4 %5 %6

