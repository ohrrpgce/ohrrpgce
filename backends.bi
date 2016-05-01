'' Header for backends.bas. See gfx.bi and music.bi for the actual interfaces

#ifndef BACKENDS_BI
#define BACKENDS_BI

declare sub init_gfx_backend()
declare function backends_setoption cdecl(opt as string, arg as string) as integer
declare sub read_backend_info()

extern wantpollingthread as bool
extern as string gfxbackend, musicbackend
extern as string gfxbackendinfo, musicbackendinfo, systeminfo
'This is shared between gfx_alleg and music_allegro
extern allegro_initialised as bool

#endif BACKENDS_BI
