'' Header for backends.bas. See gfx.bi and music.bi for the actual interfaces

#ifndef BACKENDS_BI
#define BACKENDS_BI

declare sub load_best_gfx_backend()
declare sub init_best_gfx_backend()
declare function backends_setoption (opt as string, arg as string) as integer
declare function switch_gfx_backend (name as string) as bool
declare sub read_backend_info()
declare function valid_gfx_backend (name as string) as bool

declare sub gfx_backend_menu ()
declare sub music_backend_menu ()

extern wantpollingthread as bool
extern as string gfxbackend, musicbackend
extern as string gfxbackendinfo, musicbackendinfo, systeminfo
'This is shared between gfx_alleg and music_allegro
extern allegro_initialised as bool

#endif BACKENDS_BI
