cp --reply=yes fbcompat.bi compat.bi
cp --reply=yes fbcompat.bas compat.bas
./verprint
fbc -v -m game game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas gicon.rc
mv game ohrrpgce
