-----------------------------------------------------------
FBOHR - OHRRPGCE FreeBasic version

2006-02-04 - Chomsky/Camus - Fixes and optimisations. 
		Added MIDI import.
2006-01-23 - Berkeley/Bentham - Many fixes, new graphics
		systems. Allegro version. First version
		of Custom.
2005-10-18 - Aristotle
-----------------------------------------------------------

This is a test release of the Windows version of Custom.
Think of it as a beta. Please do not risk important files.
Take backups regularly.

TWO VERSIONS

The current releases of Game and Custom come in two
flavours, using different external libraries. The first 
(berkeley) uses SDL for sound playback. The second
(bentham) uses Allegro for graphics and sound. FMODEX,
used by the original aristotle release, is no longer
supported due to licensing incompatibilities.

I have named the executables differently for each version,
so that they can all go into the same folder if desired.

berkeley: game.exe, custom.exe + sdl.dll, sdl_mixer.dll
bentham: gamea.exe, customa.exe + alleg40.dll

DIFFERENCES

camus (Allegro): 
- 640x400 double resolution
- ALT-ENTER doesn't work at all in Custom, and only in-game
  in game.
- Starts full-screen by default.

chomsky (SDL):
- Pause_Music doesn't seem to work, meaning that the songs 
  continue to play when you move off them in Import Music,
  until another tune plays or the Import menu is exited.
- Starts windowed by default.

FEATURES

ALT-ENTER switches between windowed and full-screen, albeit
not very well in the Allegro version.

Command-line switches can be used to change the startup 
behaviour in either version:

-windowed 
-fullscreen 

They should be self-explanatory. Set them in a shortcut or 
type them at a command-prompt. Other command-line arguments
such as a game name should still work.


Please see the documentation of the official version for
further details. It can be obtained from
http://www.hamsterrepublic.com/ohrrpgce.

Bug reports and comments by email to fbohr@pocketfuel.co.uk
or in the forums at Castle Paradox or OHRDev.

-------------------
Simon Bradley (PlayerOne)