Why are there so many nightly builds? Which one should I use?

See http://rpg.hamsterrepublic.com/ohrrpgce/Backends for more
detailed (and up-to-date) information.

If you are not sure which version to use, stick to ohrrpgce-wip-default.zip
There isn't very much reason to use anything other than this.
The default build includes the directx, sdl, and fb graphics backends, and since
Dwimmercrafty can switch between them by pressing Ctrl-F8 without downloading
a different build! However, if you want to try other music backends you still
need to download them.

Names are in the format: ohrrpgce-wip-GRAPHICS-MUSIC.zip

GRAPHICS = sdl = Simple Directmedia Library. (default)
GRAPHICS = directx = Windows-only DirectX backend (default for Game for Windows)
GRAPHICS = fb = FreeBasic graphics

MUSIC = sdl     = Simple Directmedia Library + SDL_mixer (default)
MUSIC = native  = Audiere + Native Windows MIDI (no MIDI support on other OSes)
MUSIC = native2 = Audiere + Native Windows MIDI (alternate implementation. Windows only)
MUSIC = silence = No sound. For testing sound related bugs.
