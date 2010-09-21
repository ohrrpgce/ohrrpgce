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
CC = None
CXX = None
CXXFLAGS = '-O2 -g -Wall -Wno-non-virtual-dtor'.split ()
from ohrbuild import basfile_scan, verprint

if platform.system () == 'Windows':
    win32 = True
    unix = False
    exe_suffix = '.exe'
    CC = 'gcc'
    CXX = 'g++'
else:
    unix = True

baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                suffix = '.o', src_suffix = '.bas')
basexe = Builder (action = '$FBC $FBFLAGS -x $TARGET $FBLIBS $SOURCES',
                  suffix = exe_suffix, src_suffix = '.bas')

env = os.environ
svn = ARGUMENTS.get ('svn','svn')
fbc = ARGUMENTS.get ('fbc','fbc')
git = ARGUMENTS.get ('git','git')
# eg. pass gfx=sdl+fb for the default behaviour.
gfx = ARGUMENTS.get ('gfx', env.get ('OHRGFX','sdl+fb'))
music = ARGUMENTS.get ('music', env.get ('OHRMUSIC','sdl'))
# handle OHRMUSIC/GFX which is blank
# (ie is set to '', rather than not existing.)
if gfx == '':
    gfx = 'sdl+fb'
if music == '':
    music = 'sdl'
env = Environment (FBFLAGS  = env.get ('FBFLAGS', []),
                   FBLIBS = [],
                   CFLAGS = ['-c','-g','-O3','--std=c99'],
                   FBC = fbc +' -lang deprecated',
                   CXXFLAGS = CXXFLAGS,
                   BUILDERS = {'BASEXE':basexe,'BASO':baso})

env['ENV']['PATH'] = os.environ['PATH']
if CC:
    env['ENV']['CC'] = CC
    env.Replace (CC = CC)

if CXX:
    env['ENV']['CXX'] = CXX
    env.Replace (CXX = CXX)

scanner = Scanner (function = basfile_scan,
                   skeys = ['.bas'])

env.Append (SCANNERS = scanner)

for f in ('-mt', '-g','-exx'):
    env['FBFLAGS'].append (f)

EXE_SUFFIX = ''
common_objects = []
common_modules = []

libraries = ['fbgfx']
libpaths = []

if win32:
    common_modules += ['blit.c','base64.c']
    env['FBFLAGS'] += ['-s', 'gui']
elif unix:
    common_modules += ['blit.c', 'base64.c']
    libraries += 'X11 Xext Xpm Xrandr Xrender pthread'.split (' ')

used_gfx = []
used_music = []

### Add various modules to build, conditional on OHRGFX and OHRMUSIC

gfx_map = {'fb': {'common_modules': 'gfx_fb.bas'},
           'alleg' : {'common_modules': 'gfx_alleg.bas', 'libraries': 'alleg'},
           'sdl' : {'common_modules': 'gfx_sdl.bas', 'libraries': 'SDL'},
           'directx' : {}, # nothing needed
           'sdlpp': {}     # nothing needed
           }

music_map = {'native':
                 {'common_modules': 'music_native.bas',
                  'common_objects': os.path.join ('audwrap','audwrap.o'),
                  'libraries': 'audiere',
                  'libpaths': 'audwrap'},
             'native2':
                 {'common_modules': 'music_native2.bas',
                  'common_objects': os.path.join ('audwrap','audwrap.o'),
                  'libraries': 'audiere',
                  'libpaths': 'audwrap'},
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

common_modules += [v + '.bas' for v in Split ("""allmodex
                   backends
                   lumpfile
                   compat
                   bam2mid
                   common
                   browse
                   util
                   loading
                   reload
                   reloadext
                   slices""")]


edit_modules = ['custom',
                'customsubs',
                'drawing',
                'subs',
                'subs2',
                'mapsubs',
                'flexmenu',
                'menus',
                'sliceedit']

edit_modules.reverse ()

game_modules = ['game',
                'bmod',
                'bmodsubs',
                'menustuf',
                'moresubs',
                'yetmore',
                'yetmore2',
                'savegame',
                'hsinterpreter']

game_modules.reverse ()

semicommon_modules = ['backends.bas',
                      'browse.bas',
                      'common.bas',
                      'allmodex.bas',
                      'slices.bas',
                      'compat.bas',
                      'music_native.bas',
                      'music_native2.bas']

_libraries = libraries
libraries = []
_libpaths = libpaths
libpaths = []
for v2 in [['-l', v] for v in _libraries]:
    libraries.extend (v2)
for v2 in [['-p', v] for v in _libpaths]:
    libpaths.extend (v2)


# Make an environment suitable for building the main stuff..

main = env.Clone ()

main['FBLIBS'] += libpaths + libraries


# first, make sure the version is saved.

# always do verprinting, before anything else.
verprint (used_gfx, used_music, svn, git, fbc)

semicommon_modules.pop ()
semicommon_modules.pop ()

gameenv = main.Clone (FBFLAGS = env['FBFLAGS'] + \
                      ['-d','IS_GAME', '-m','game'])
editenv = main.Clone (FBFLAGS = env['FBFLAGS'] + \
                      ['-d','IS_CUSTOM', '-m','custom'])

gametmp = []
edittmp = []
tmp = common_modules + common_objects
for v in semicommon_modules:
    if v not in tmp:
        tmp.append (v)
for v in tmp:
    if v.endswith ('.c'):
        tmp = main.Command (v.replace ('.c','.o'),
         v,
         '$CC $CFLAGS -c $SOURCE -o $TARGET')
        if v == 'base64.c':
            Depends (tmp, 'base64.h')
        gametmp.append (tmp)
        edittmp.append (tmp)
    elif v.endswith ('.bas'):
        a = gameenv.BASO (target = 'game-'+ v[:-4], source = v,)
        b = editenv.BASO (target = 'edit-'+ v[:-4], source = v,)
        gametmp.append (a)
        edittmp.append (b)
        Depends (a,'gver.txt')
        Depends (b,'cver.txt')
    #else:

        #gametmp.append (v.replace ('.c','.o'))
     #   edittmp.append (v.replace ('.c','.o'))

BAM2MID = env.BASEXE (os.path.join ('..','bam2mid'))
Default (BAM2MID)

#now... GAME and CUSTOM
#

gamesrc = []
editsrc = []

for item in game_modules:
    a = gameenv.BASO (target = item + '.o', source = item + '.bas')
    Depends (a,'gver.txt')
    gamesrc.append (a)

for item in edit_modules:
    b = editenv.BASO (target = item + '.o', source = item + '.bas')
    Depends (b,'cver.txt')
    editsrc.append (b)

mainflags = ['-v'] + env['FBFLAGS']
gamename = 'ohrrpgce-game'
editname = 'ohrrpgce-custom'
gameflags = list (mainflags)
editflags = list (mainflags)

if win32:
    gamename = 'game'
    editname = 'custom'
    gameflags += ['gicon.rc']
    editflags += ['cicon.rc']
else:
    gameflags += ['-d', 'DATAFILES="/usr/share/games/ohrrpgce"']
    editflags += ['-d', 'DATAFILES="/usr/share/games/ohrrpgce"']

gamename = os.path.join ('..', gamename)
editname = os.path.join ('..', editname)

GAME = gameenv.BASEXE   (gamename,
                         FBFLAGS = gameflags,
                         source = gametmp + gamesrc)
CUSTOM = editenv.BASEXE (editname,
                         FBFLAGS = editflags,
                         source = edittmp + editsrc)
BAM2MID = env.BASEXE (os.path.join ('..','bam2mid'))

audwrap = env.Command (os.path.join ('audwrap', 'audwrap.o'),
                       os.path.join ('audwrap', 'audwrap.cpp'),
                       '$CXX -c $SOURCE -o $TARGET $CXXFLAGS')
Depends (audwrap, 'gver.txt')
Depends (audwrap, 'cver.txt')
Depends (audwrap, os.path.join ('audwrap', 'audwrap.h'))
AUDWRAP = env.Library (os.path.join ('audwrap','audwrap'),
          source = audwrap)
# XXX fix verprint build to happen in correct place??
if 'native' in used_music or 'native2' in used_music:
    Default (AUDWRAP)
Default (GAME)
Default (CUSTOM)
Default (BAM2MID)

