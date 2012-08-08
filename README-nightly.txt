Why are there so many nightly builds? Which one should I use?

Names are in the format: ohrrpgce-wip-GRAPHICS-MUSIC.zip

GRAPHICS = sdl = Simple Directmedia Library. (default)
GRAPHICS = directx = Windows-only DirectX backend (default for Windows)
GRAPHICS = fb = FreeBasic graphics
GRAPHICS = alleg = Allegro Game Library (not recommended)

MUSIC = sdl     = Simple Directmedia Library + SDL_mixer (default)
MUSIC = native  = Audiere + Native Windows MIDI
MUSIC = native2 = Audiere + Native Windows MIDI (alternate implementation)
MUSIC = silence = No sound. For testing sound related bugs.

See http://rpg.hamsterrepublic.com/ohrrpgce/Backends for more
detailed (and up-to-date) information.

If you are not sure which version to use, try ohrrpgce-wip-default.zip
