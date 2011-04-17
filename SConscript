#!/usr/bin/env python
"""Main scons build script for OHRRPGCE

cf. SConstruct, ohrbuild.py
"""
import os
import platform
from ohrbuild import basfile_scan, verprint

win32 = False
unix = True
exe_suffix = ''
CC = 'gcc'
CXX = 'g++'
CXXFLAGS = '-O2 -g -Wall -Wno-non-virtual-dtor'.split ()
from ohrbuild import basfile_scan, verprint

if platform.system () == 'Windows':
    win32 = True
    unix = False
    exe_suffix = '.exe'
else:
    unix = True


environ = os.environ
svn = ARGUMENTS.get ('svn','svn')
fbc = ARGUMENTS.get ('fbc','fbc')
git = ARGUMENTS.get ('git','git')
# eg. pass gfx=sdl+fb for the default behaviour.
if unix:
    gfx = ARGUMENTS.get ('gfx', environ.get ('OHRGFX','sdl+fb'))
else:
    gfx = ARGUMENTS.get ('gfx', environ.get ('OHRGFX','directx+sdl+fb'))
music = ARGUMENTS.get ('music', environ.get ('OHRMUSIC','sdl'))
# handle OHRMUSIC/GFX which is blank
# (ie is set to '', rather than not existing.)
if gfx == '':
    gfx = 'sdl+fb'
if music == '':
    music = 'sdl'
env = Environment (FBFLAGS = environ.get ('FBFLAGS', []) + ['-mt','-g','-exx'],
                   FBLIBS = [],
                   CFLAGS = ['-c','-g','-O3','--std=c99'],
                   FBC = fbc + ' -lang deprecated',
                   CXXFLAGS = CXXFLAGS,
                   VAR_PREFIX = '')


def prefix_targets(target, source, env):
    target = [File(env['VAR_PREFIX'] + str(a)) for a in target]
    return target, source

variant_baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                suffix = '.o', src_suffix = '.bas', single_source = True, emitter = prefix_targets)
baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                suffix = '.o', src_suffix = '.bas', single_source = True)
basexe = Builder (action = '$FBC $FBFLAGS -x $TARGET $FBLIBS $SOURCES',
                  suffix = exe_suffix, src_suffix = '.bas')

scanner = Scanner (function = basfile_scan,
                   skeys = ['.bas'])

env['BUILDERS']['Object'].add_action('.bas', '$FBC -c $SOURCE -o $TARGET $FBFLAGS')

env.Append (BUILDERS = {'BASEXE':basexe, 'BASO':baso, 'VARIANT_BASO':variant_baso},
            SCANNERS = scanner)


env['ENV']['PATH'] = os.environ['PATH']
if CC:
    env['ENV']['CC'] = CC
    env.Replace (CC = CC)

if CXX:
    env['ENV']['CXX'] = CXX
    env.Replace (CXX = CXX)

# Make a base environment for Game and Custom (other utilities use env)
commonenv = env.Clone ()

base_objects = []   # modules shared by all utilities (except bam2mid)
common_modules = []  # modules, in addition to base_objects, shared by Game and Custom 
common_objects = []

libraries = []
libpaths = []

if win32:
    base_objects += ['os_windows.bas']
    libraries += ['fbgfx']
    commonenv['FBFLAGS'] += ['-s','gui']
elif unix:
    base_objects += ['os_unix.bas']
    libraries += 'X11 Xext Xpm Xrandr Xrender pthread'.split (' ')
    commonenv['FBFLAGS'] += ['-d', 'DATAFILES=\'"/usr/share/games/ohrrpgce"\'']

used_gfx = []
used_music = []

### Add various modules to build, conditional on OHRGFX and OHRMUSIC

gfx_map = {'fb': {'common_modules': 'gfx_fb.bas', 'libraries': 'fbgfx'},
           'alleg' : {'common_modules': 'gfx_alleg.bas', 'libraries': 'alleg'},
           'sdl' : {'common_modules': 'gfx_sdl.bas', 'libraries': 'SDL'},
           'directx' : {}, # nothing needed
           'sdlpp': {}     # nothing needed
           }

music_map = {'native':
                 {'common_modules': 'music_native.bas',
                  'common_objects': os.path.join ('audwrap','audwrap.cpp'),
                  'libraries': 'audiere'},
             'native2':
                 {'common_modules': 'music_native2.bas',
                  'common_objects': os.path.join ('audwrap','audwrap.cpp'),
                  'libraries': 'audiere'},
             'sdl':
                 {'common_modules': 'music_sdl.bas sdl_lumprwops.bas',
                  'libraries': 'SDL SDL_mixer'},
             'silence':
                 {'common_modules': 'music_silence.bas'}
            }

tmp = globals ()
gfx = gfx.split ("+")
for k in gfx:
    if k not in used_gfx:
        used_gfx.append (k)
        for k2, v2 in gfx_map[k].items ():
            tmp[k2] += v2.split (' ')

for k, v in music_map.items ():
    if k == music:
        if k not in used_music:
            used_music.append (k)
        for k2, v2 in v.items ():
            tmp[k2] += v2.split (' ')

libraries = Flatten ([['-l', v] for v in libraries])
libpaths = Flatten ([['-p', v] for v in libpaths])

commonenv['FBLIBS'] += libpaths + libraries

# first, make sure the version is saved.

# always do verprinting, before anything else.
verprint (used_gfx, used_music, svn, git, fbc)


base_objects += ['util.bas','blit.c','base64.c']

common_modules += ['allmodex',
                   'backends',
                   'lumpfile',
                   'misc',
                   'bam2mid',
                   'common',
                   'bcommon',
                   'browse',
                   'loading',
                   'reload',
                   'reloadext',
                   'slices']

edit_modules = ['custom',
                'customsubs',
                'drawing',
                'subs',
                'subs2',
                'mapsubs',
                'flexmenu',
                'menus',
                'sliceedit',
                'reloadedit',
                'editedit',
                'editrunner']

game_modules = ['game',
                'bmod',
                'bmodsubs',
                'menustuf',
                'moresubs',
                'yetmore',
                'yetmore2',
                'savegame',
                'hsinterpreter']

base_objects = [env.Object(a) for a in base_objects]
common_objects = [env.Object(a) for a in common_objects]

gameenv = commonenv.Clone (VAR_PREFIX = 'game-', FBFLAGS = env['FBFLAGS'] + \
                      ['-d','IS_GAME', '-m','game'])
editenv = commonenv.Clone (VAR_PREFIX = 'edit-', FBFLAGS = env['FBFLAGS'] + \
                      ['-d','IS_CUSTOM', '-m','custom'])

#now... GAME and CUSTOM

gamesrc = base_objects + common_objects
for item in game_modules:
    gamesrc.append (gameenv.BASO (item))
for item in common_modules:
    gamesrc.append (gameenv.VARIANT_BASO (item))

editsrc = base_objects + common_objects
for item in edit_modules:
    editsrc.append (editenv.BASO (item))
for item in common_modules:
    editsrc.append (editenv.VARIANT_BASO (item))

# For reload utilities
reload_objects = base_objects + [env.BASO (item) for item in ['reload', 'reloadext', 'lumpfile']]

gamename = 'ohrrpgce-game'
editname = 'ohrrpgce-custom'
gameflags = list (gameenv['FBFLAGS']) + ['-v']
editflags = list (editenv['FBFLAGS']) + ['-v']

if win32:
    gamename = 'game'
    editname = 'custom'
    gamesrc += ['gicon.rc']
    editsrc += ['cicon.rc']

GAME = gameenv.BASEXE   (gamename, source = gamesrc, FBFLAGS = gameflags)
CUSTOM = editenv.BASEXE (editname, source = editsrc, FBFLAGS = editflags)
env.BASEXE ('bam2mid')
env.BASEXE ('unlump', source = ['unlump.bas', 'lumpfile.bas'] + base_objects)
env.BASEXE ('relump', source = ['relump.bas', 'lumpfile.bas'] + base_objects)
env.Command ('hspeak', source = ['hspeak.exw', 'hsspiffy.e'], action = 'euc hspeak.exw')
env.BASEXE ('reloadtest', source = ['reloadtest.bas'] + reload_objects)
env.BASEXE ('xml2reload', source = ['xml2reload.bas'] + reload_objects, FB_FLAGS = ['-p','.', '-l','xml2'])
env.BASEXE ('reload2xml', source = ['reload2xml.bas'] + reload_objects)
env.BASEXE ('reloadutil', source = ['reloadutil.bas'] + reload_objects)

Default (GAME)
Default (CUSTOM)

Alias ('game', GAME)
Alias ('custom', CUSTOM)

#print [str(a) for a in FindSourceFiles(GAME)]
