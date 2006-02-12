#!/bin/sh
cp -f fbcompat.bi compat.bi
cp -f fbcompat.bas compat.bas
./verprint
fbc -v -m game -d IS_GAME \
  game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas gfx_fb2.bas music_sdl.bas gicon.rc \
&& mv game ohrrpgce-game
