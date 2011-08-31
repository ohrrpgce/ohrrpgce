#!/usr/bin/env python
"""Main scons build script for OHRRPGCE

cf. SConstruct, ohrbuild.py
"""
import os
import platform
import shutil
from ohrbuild import basfile_scan, verprint

win32 = False
unix = True
exe_suffix = ''
FBFLAGS = os.environ.get ('FBFLAGS', []) + ['-mt']
#CC and CXX are probably not needed anymore
CC = ''
CXX = ''
CFLAGS = '-g -Wall --std=c99'.split ()
CXXFLAGS = '-g -Wall -Wno-non-virtual-dtor'.split ()
C_opt = True    # compile with -O2?
FB_exx = True   # compile with -exx?
FB_g = True   # compile with -g?
envextra = {}
from ohrbuild import basfile_scan, verprint

if platform.system () == 'Windows':
    win32 = True
    unix = False
    exe_suffix = '.exe'
    # Force use of gcc instead of MSVC++, so compiler flags are understood
    envextra = {'tools': ['mingw']}
else:
    unix = True

if 'gengcc' in ARGUMENTS:
    FBFLAGS += ["-gen", "gcc"]

environ = os.environ
svn = ARGUMENTS.get ('svn','svn')
fbc = ARGUMENTS.get ('fbc','fbc')
git = ARGUMENTS.get ('git','git')
if 'debug' in ARGUMENTS:
    C_opt = not int (ARGUMENTS['debug'])
    FB_g = FB_exx = int (ARGUMENTS['debug'])
if 'profile' in ARGUMENTS:
    FBFLAGS.append ('-profile')
    CFLAGS.append ('-pg')
    CXXFLAGS.append ('-pg')
if 'scriptprofile' in ARGUMENTS:
    FBFLAGS += ['-d','SCRIPTPROFILE']
if ARGUMENTS.get ('valgrind', 0):
    #-exx under valgrind is nearly redundant, and really slow
    FB_exx = False
    CFLAGS.append ('-DVALGRIND_ARRAYS')
if FB_exx:
    FBFLAGS.append ('-exx')
if FB_exx:
    FBFLAGS.append ('-g')
if C_opt:
    CFLAGS.append ('-O2')
    CXXFLAGS.append ('-O3')
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
env = Environment (FBFLAGS = FBFLAGS,
                   FBLIBS = [],
                   CFLAGS = CFLAGS,
                   FBC = fbc + ' -lang deprecated',
                   CXXFLAGS = CXXFLAGS,
                   CXXLINKFLAGS = [],
                   VAR_PREFIX = '',
                   **envextra)

def prefix_targets(target, source, env):
    target = [File(env['VAR_PREFIX'] + str(a)) for a in target]
    return target, source

#variant_baso creates Nodes/object files with filename prefixed with VAR_PREFIX environment variable
variant_baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                suffix = '.o', src_suffix = '.bas', single_source = True, emitter = prefix_targets)
baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                suffix = '.o', src_suffix = '.bas', single_source = True)
basexe = Builder (action = '$FBC $FBFLAGS -x $TARGET $FBLIBS $SOURCES',
                  suffix = exe_suffix, src_suffix = '.bas')

bas_scanner = Scanner (function = basfile_scan,
                       skeys = ['.bas', '.bi'], recursive = True)

env['BUILDERS']['Object'].add_action ('.bas', '$FBC -c $SOURCE -o $TARGET $FBFLAGS')
SourceFileScanner.add_scanner ('.bas', bas_scanner)
SourceFileScanner.add_scanner ('.bi', bas_scanner)

env.Append (BUILDERS = {'BASEXE':basexe, 'BASO':baso, 'VARIANT_BASO':variant_baso},
            SCANNERS = bas_scanner)


env['ENV']['PATH'] = os.environ['PATH']
if CC:
    env['ENV']['CC'] = CC
    env.Replace (CC = CC)

if CXX:
    env['ENV']['CXX'] = CXX
    env.Replace (CXX = CXX)

# Make a base environment for Game and Custom (other utilities use env)
commonenv = env.Clone ()

base_modules = []   # modules shared by all utilities (except bam2mid)
shared_modules = []  # freebasic modules shared by, but with separate builds, for Game and Custom 
common_modules = []  # other modules (in any language) shared by Game and Custom
common_objects = []  # other objects shared by Game and Custom

libraries = []
libpaths = []

if win32:
    base_modules += ['os_windows.bas']
    libraries += ['fbgfx']
    commonenv['FBFLAGS'] += ['-s','gui']
elif unix:
    base_modules += ['os_unix.c']
    libraries += 'X11 Xext Xpm Xrandr Xrender pthread'.split (' ')
    commonenv['FBFLAGS'] += ['-d', 'DATAFILES=\'"/usr/share/games/ohrrpgce"\'']

used_gfx = []
used_music = []

### Add various modules to build, conditional on OHRGFX and OHRMUSIC

gfx_map = {'fb': {'shared_modules': 'gfx_fb.bas', 'libraries': 'fbgfx'},
           'alleg' : {'shared_modules': 'gfx_alleg.bas', 'libraries': 'alleg'},
           'sdl' : {'shared_modules': 'gfx_sdl.bas', 'libraries': 'SDL'},
           'directx' : {}, # nothing needed
           'sdlpp': {}     # nothing needed
           }

music_map = {'native':
                 {'shared_modules': 'music_native.bas',
                  'common_modules': os.path.join ('audwrap','audwrap.cpp'),
                  'libraries': 'audiere'},
             'native2':
                 {'shared_modules': 'music_native2.bas',
                  'common_modules': os.path.join ('audwrap','audwrap.cpp'),
                  'libraries': 'audiere'},
             'sdl':
                 {'shared_modules': 'music_sdl.bas sdl_lumprwops.bas',
                  'libraries': 'SDL SDL_mixer', 'libpaths': 'win32'},
             'silence':
                 {'shared_modules': 'music_silence.bas'}
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

commonenv['CXXLINKFLAGS'] += ['-l' + lib for lib in libraries]
commonenv['CXXLINKFLAGS'] += ['-L' + path for path in libpaths]

libraries = Flatten ([['-l', v] for v in libraries])
libpaths = Flatten ([['-p', v] for v in libpaths])

commonenv['FBLIBS'] += libpaths + libraries

# first, make sure the version is saved.

# always do verprinting, before anything else.
verprint (used_gfx, used_music, svn, git, fbc)


base_modules += ['util.bas', 'blit.c', 'base64.c', 'array.c', 'vector.bas']

shared_modules += ['allmodex',
                   'backends',
                   'lumpfile',
                   'misc',
                   'bam2mid',
                   'common',
                   'bcommon',
                   'menus',
                   'browse',
                   'loading',
                   'reload',
                   'reloadext',
                   'sliceedit',
                   'slices']

edit_modules = ['custom',
                'customsubs',
                'drawing',
                'subs',
                'subs2',
                'subs4',
                'mapsubs',
                'flexmenu',
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

common_modules += ['filelayer.cpp']

if 'raster' in ARGUMENTS:
    common_modules += ['rasterizer.cpp', 'matrixMath.cpp', 'gfx_newRenderPlan.cpp']
    commonenv['FBFLAGS'] += ['-d', 'USE_RASTERIZER']

def which(env, prog_name):
    "Like the 'which' utility, using env['ENV']['PATH']"
    if win32:
        paths = env['ENV']['PATH'].split(';')
    else:
        paths = env['ENV']['PATH'].split(':')
    for path in paths:
        name = os.path.abspath(os.path.join(path, prog_name)) + exe_suffix
        #print "trying " + path + " = " + name
        if os.path.isfile(name):
            return name
    return None

def get_run_command(cmd):
    """Runs a shell commands and returns stdout as a string"""
    import subprocess
    proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    errtext = proc.stderr.read()
    if len(errtext) > 0:
        raise Exception("subprocess.Popen(%s) returned stderr:\n%s" % (cmd, errtext))
    return proc.stdout.read().strip()

if 'linkgcc' in ARGUMENTS:
    fbc_exe = which(commonenv, fbc)
    #print "fbc = " + os.path.dirname(fbc_exe)
    import re
    fbcinfo = get_run_command("fbc -version")
    target = re.findall("target:([a-z]*)", fbcinfo)
    if len(target) == 0:
        raise Exception("Couldn't determine fbc target")

    if not win32:
        commonenv['CXXLINKFLAGS'] += ['linux/fb_icon.c']
    
    libpath = os.path.join(os.path.dirname(fbc_exe), 'lib', target[0])
    commonenv['CXXLINKFLAGS'] += ['-L' + libpath, os.path.join(libpath, 'fbrt0.o'), '-lfbmt', '-lncurses']

    basexe_gcc = Builder (action = '$CXX $CXXFLAGS -o $TARGET $SOURCES $CXXLINKFLAGS',
                  suffix = exe_suffix, src_suffix = '.bas')

    commonenv['BUILDERS']['BASEXE'] = basexe_gcc

elif win32:
    if not os.path.isfile('libgcc_s.a'):
        shutil.copy(get_run_command("gcc -print-file-name=libgcc_s.a"), ".")
    if not os.path.isfile('libstdc++.a'):
        shutil.copy(get_run_command("gcc -print-file-name=libstdc++.a"), ".")
    #commonenv['FBLIBS'] += ['-l','gcc_s','-l','stdc++']

# Note that base_objects are not built in commonenv!
base_objects = [env.Object(a) for a in base_modules]
common_objects += base_objects + [commonenv.Object(a) for a in common_modules]
# Plus unique module included by utilities but not Game or Custom
base_objects.append (env.Object ('common_base.bas'))


gameenv = commonenv.Clone (VAR_PREFIX = 'game-', FBFLAGS = commonenv['FBFLAGS'] + \
                      ['-d','IS_GAME', '-m','game'])
editenv = commonenv.Clone (VAR_PREFIX = 'edit-', FBFLAGS = commonenv['FBFLAGS'] + \
                      ['-d','IS_CUSTOM', '-m','custom'])

#now... GAME and CUSTOM

gamesrc = common_objects[:]
for item in game_modules:
    gamesrc.append (gameenv.BASO (item))
for item in shared_modules:
    gamesrc.append (gameenv.VARIANT_BASO (item))

editsrc = common_objects[:]
for item in edit_modules:
    editsrc.append (editenv.BASO (item))
for item in shared_modules:
    editsrc.append (editenv.VARIANT_BASO (item))

# For reload utilities
reload_objects = base_objects + [env.BASO (item) for item in ['reload', 'reloadext', 'lumpfile']]

gamename = 'ohrrpgce-game'
editname = 'ohrrpgce-custom'
gameflags = list (gameenv['FBFLAGS']) #+ ['-v']
editflags = list (editenv['FBFLAGS']) #+ ['-v']

if win32:
    gamename = 'game'
    editname = 'custom'
    if 'linkgcc' in ARGUMENTS:
        # FIXME: This is a stopgap, it only works if the .rc files have previously been compiled
        gamesrc += ['gicon.obj']
        editsrc += ['cicon.obj']
    else:
        gamesrc += ['gicon.rc']
        editsrc += ['cicon.rc']

GAME = gameenv.BASEXE   (gamename, source = gamesrc, FBFLAGS = gameflags)
CUSTOM = editenv.BASEXE (editname, source = editsrc, FBFLAGS = editflags)
env.BASEXE ('bam2mid')
env.BASEXE ('unlump', source = ['unlump.bas', 'lumpfile.o'] + base_objects)
env.BASEXE ('relump', source = ['relump.bas', 'lumpfile.o'] + base_objects)
env.BASEXE ('dumpohrkey')
env.Command ('hspeak', source = ['hspeak.exw', 'hsspiffy.e'], action = 'euc -gcc hspeak.exw')
RELOADTEST = env.BASEXE ('reloadtest', source = ['reloadtest.bas'] + reload_objects)
XML2RELOAD = env.BASEXE ('xml2reload', source = ['xml2reload.bas'] + reload_objects, FBLIBS = env['FBLIBS'] + ['-p','.', '-l','xml2'])
RELOAD2XML = env.BASEXE ('reload2xml', source = ['reload2xml.bas'] + reload_objects)
RELOADUTIL = env.BASEXE ('reloadutil', source = ['reloadutil.bas'] + reload_objects)
env.BASEXE ('vectortest', source = ['vectortest.bas'] + base_objects)

Default (GAME)
Default (CUSTOM)

Alias ('game', GAME)
Alias ('custom', CUSTOM)
Alias ('reload', [RELOADUTIL, RELOAD2XML, XML2RELOAD, RELOADTEST])

#print [str(a) for a in FindSourceFiles(GAME)]

Help ("""
Usage:  scons [SCons options] [options] [targets]

Options:
  gfx=BACKENDS        Graphics backends, concatenated with +. Options:
                        """ + " ".join (gfx_map.keys ()) + """
                      At runtime, backends are tried in the order specified.
                      Current (default) value: """ + "+".join (gfx) + """
  music=BACKEND       Music backend. Options:
                        """ + " ".join (music_map.keys ()) + """
                      Current (default) value: """ + music + """
  debug=0|1           Debugging build: with -exx and without optimisation.
                      Set to 0 to force building without -exx.
  valgrind=1          valgrinding build.
  profile=1           Profiling build for gprof.
  scriptprofile=1     Script profiling build.
  fbc=PATH            Override fbc.
  svn=PATH            Override svn.
  git=PATH            Override git.

Experimental options:
  raster=1            Include new graphics API and rasterizer.
  gengcc=1            Compile using GCC emitter.
  linkgcc=1           Link using g++ instead of fbc.

Targets:
  """ + gamename + """ (or game)
  """ + editname + """ (or custom)
  unlump
  relump
  hspeak
  reloadtest
  xml2reload
  reload2xml
  reloadutil
  vectortest
  dumpohrkey
  bam2mid
  reload              Compile all RELOAD utilities.
  .                   Compile everything.

With no targets specified, compiles game and custom.

Examples:
  scons
  scons gfx=sdl+fb music=native game custom
  scons -j 2 debug=1 .
""")
