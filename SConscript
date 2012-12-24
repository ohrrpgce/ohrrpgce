#!/usr/bin/env python
"""Main scons build script for OHRRPGCE

cf. SConstruct, ohrbuild.py
"""
import os
import platform
import shutil
from ohrbuild import basfile_scan, verprint, get_run_command

FBFLAGS = os.environ.get ('FBFLAGS', []) + ['-mt']
#CC and CXX are probably not needed anymore
CC = ''
CXX = ''
CFLAGS = '-m32 -g -Wall --std=c99'.split ()
CXXFLAGS = '-m32 -g -Wall -Wno-non-virtual-dtor'.split ()
C_opt = True    # compile with -O2?
FB_exx = True   # compile with -exx?
FB_g = True   # compile with -g?
linkgcc = False  # link using g++?
GCC_strip = False  # (linkgcc only) strip (link with -s)?
envextra = {}
FRAMEWORKS_PATH = "~/Library/Frameworks"  # Frameworks search path in addition to the default /Library/Frameworks

win32 = False
unix = False
mac = False
exe_suffix = ''
if platform.system () == 'Windows':
    win32 = True
    exe_suffix = '.exe'
    # Force use of gcc instead of MSVC++, so compiler flags are understood
    envextra = {'tools': ['mingw']}
elif platform.system () == 'Darwin':
    unix = True
    mac = True
else:
    unix = True

if 'asm' in ARGUMENTS:
    FBFLAGS += ["-r", "-g"]

if 'gengcc' in ARGUMENTS:
    FBFLAGS += ["-gen", "gcc"]

if 'deprecated' in ARGUMENTS:
    FBFLAGS += ["-d", "LANG_DEPRECATED"]

linkgcc = int (ARGUMENTS.get ('linkgcc', True))

environ = os.environ
fbc = ARGUMENTS.get ('fbc','fbc')
if 'debug' in ARGUMENTS:
    GCC_strip = C_opt = not int (ARGUMENTS['debug'])
    FB_g = FB_exx = int (ARGUMENTS['debug'])
if 'profile' in ARGUMENTS:
    FBFLAGS.append ('-profile')
    CFLAGS.append ('-pg')
    CXXFLAGS.append ('-pg')
    FB_g = True
    GCC_strip = False
if 'scriptprofile' in ARGUMENTS:
    FBFLAGS += ['-d','SCRIPTPROFILE']
if ARGUMENTS.get ('valgrind', 0):
    #-exx under valgrind is nearly redundant, and really slow
    FB_exx = False
    CFLAGS.append ('-DVALGRIND_ARRAYS')
if FB_exx:
    FBFLAGS.append ('-exx')
if FB_g:
    FBFLAGS.append ('-g')
if C_opt:
    CFLAGS.append ('-O2')
    CXXFLAGS.append ('-O3')
# Backend selection.
if mac:
    gfx = 'sdl'
elif unix:
    gfx = 'sdl+fb'
elif win32:
    gfx = 'directx+sdl+fb'
gfx = ARGUMENTS.get ('gfx', environ.get ('OHRGFX', gfx))
gfx = gfx.split ("+")
gfx = [g.lower () for g in gfx]
music = ARGUMENTS.get ('music', environ.get ('OHRMUSIC','sdl'))
music = [music.lower ()]

env = Environment (FBFLAGS = FBFLAGS,
                   FBLIBS = [],
                   CFLAGS = CFLAGS,
                   FBC = fbc + ' -lang fb',
                   CXXFLAGS = CXXFLAGS,
                   CXXLINKFLAGS = [],
                   VAR_PREFIX = '',
                   **envextra)

# Shocked that scons doesn't provide $HOME
# $DISPLAY is need for both gfx_sdl and gfx_fb
for var in 'PATH', 'DISPLAY', 'HOME', 'EUDIR':
    if var in os.environ:
        env['ENV'][var] = os.environ[var]

if win32:
    w32_env = Environment ()

    if 'DXSDK_DIR' in os.environ:
        w32_env.Append(CPPPATH = os.path.join(os.environ['DXSDK_DIR'], 'Include'))
        w32_env.Append(LIBPATH = os.path.join(os.environ['DXSDK_DIR'], 'Lib', 'x86'))

if mac:
    macsdk = ARGUMENTS.get ('macsdk', '')
    macSDKpath = ''
    env['FBLIBS'] += ['-Wl', '-F,' + FRAMEWORKS_PATH]
    env['CXXLINKFLAGS'] += ['-F', FRAMEWORKS_PATH]
    if macsdk:
        macSDKpath = 'MacOSX' + macsdk + '.sdk'
        if macsdk == '10.4':
            macSDKpath = 'MacOSX10.4u.sdk'
        macSDKpath = '/Developer/SDKs/' + macSDKpath
        if not os.path.isdir(macSDKpath):
            raise Exception('Mac SDK ' + macsdk + ' not installed: ' + macSDKpath + ' is missing')
        env['FBLIBS'] += ['-Wl', '-mmacosx-version-min=' + macsdk]
        env['CFLAGS'] += ['-mmacosx-version-min=' + macsdk]
        env['CXXFLAGS'] += ['-mmacosx-version-min=' + macsdk]


def prefix_targets(target, source, env):
    target = [File(env['VAR_PREFIX'] + str(a)) for a in target]
    return target, source

def translate_rb(source):
    if source.endswith('.rbas'):
        return env.RB(source)
    return File(source)

#variant_baso creates Nodes/object files with filename prefixed with VAR_PREFIX environment variable
variant_baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                        suffix = '.o', src_suffix = '.bas', single_source = True, emitter = prefix_targets,
                        source_factory = translate_rb)
baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                suffix = '.o', src_suffix = '.bas', single_source = True, source_factory = translate_rb)
basmaino = Builder (action = '$FBC -c $SOURCE -o $TARGET -m ${SOURCE.filebase} $FBFLAGS',
                    suffix = '.o', src_suffix = '.bas', single_source = True,
                    source_factory = translate_rb)
basexe = Builder (action = '$FBC $FBFLAGS -x $TARGET $FBLIBS $SOURCES',
                  suffix = exe_suffix, src_suffix = '.bas')

basasm = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS -r -g',
                suffix = '.asm', src_suffix = '.bas', single_source = True)

# Surely there's a simpler way to do this
def depend_on_reloadbasic_py(target, source, env):
    return (target, source + ['reloadbasic/reloadbasic.py'])

rbasic_builder = Builder (action = [[File('reloadbasic/reloadbasic.py'), '$SOURCE', '-o', '$TARGET']],
                          suffix = '.rbas.bas', src_suffix = '.rbas', emitter = depend_on_reloadbasic_py)

# windres is part of mingw, and this is only used with linkgcc anyway.
# FB includes GoRC.exe, but finding that file is too much trouble...
rc_builder = Builder (action = 'windres --input $SOURCE --output $TARGET',
                      suffix = '.obj', src_suffix = '.rc')

bas_scanner = Scanner (function = basfile_scan,
                       skeys = ['.bas', '.bi'], recursive = True)

env['BUILDERS']['Object'].add_action ('.bas', '$FBC -c $SOURCE -o $TARGET $FBFLAGS')
SourceFileScanner.add_scanner ('.bas', bas_scanner)
SourceFileScanner.add_scanner ('.bi', bas_scanner)

env.Append (BUILDERS = {'BASEXE':basexe, 'BASO':baso, 'BASMAINO':basmaino, 'VARIANT_BASO':variant_baso,
                        'RB':rbasic_builder, 'RC':rc_builder, 'ASM':basasm},
            SCANNERS = bas_scanner)

if CC:
    env['ENV']['CC'] = CC
    env.Replace (CC = CC)

if CXX:
    env['ENV']['CXX'] = CXX
    env.Replace (CXX = CXX)

if linkgcc:
    fbc_binary = env.WhereIs(fbc)
    if not fbc_binary:
        raise Exception("FreeBasic compiler is not installed!")
    fbc_path = os.path.dirname(fbc_binary)
    #print "fbc = " + fbc_path
    if win32:
        target = 'win32'
    else:
        import re
        fbcinfo = get_run_command("fbc -version")
        target = re.findall("target:([a-z]*)", fbcinfo)
        if len(target) == 0:
            target = re.findall("\) for ([a-z0-9]+)\n", fbcinfo)
            if len(target) == 0:
                raise Exception("Couldn't determine fbc target")
        target = target[0]
    
    fblibpaths = [[fbc_path, 'lib', target],
                  [fbc_path, '..', 'lib', target],
                  [fbc_path, '..', 'lib', 'freebasic', target],
                  ['/usr/share/freebasic/lib', target],
                  ['/usr/local/lib/freebasic', target],
                  ['/usr/local/lib/freebasic']]
    fblibpaths = [os.path.join(*pathparts) for pathparts in fblibpaths]
    for path in fblibpaths:
        #print "Looking for FB libs in", path
        if os.path.isfile(os.path.join(path, 'fbrt0.o')):
            libpath = path
            break
    else:
        raise Exception("Couldn't find the FreeBASIC lib directory")

    # This causes ld to recursively search the dependencies of linked dynamic libraries
    # for more dependencies (specifically SDL on X11, etc)
    # Usually the default, but overridden on some distros. Don't know whether GOLD ld supports this.
    if not mac:
        env['CXXLINKFLAGS'] += ['-Wl,--add-needed']

    # Passing this -L option straight to the linker is necessary, otherwise gcc gives it
    # priority over the default library paths, which on Windows means using FB's old mingw libraries
    env['CXXLINKFLAGS'] += ['-Wl,-L' + libpath, os.path.join(libpath, 'fbrt0.o'), '-lfbmt']
    if GCC_strip:
        env['CXXLINKFLAGS'] += ['-s']
    if win32:
        # win32\ld_opt_hack.txt contains --stack option which can't be passed using -Wl
        env['CXXLINKFLAGS'] += ['-static-libgcc', '-static-libstdc++', '-Wl,@win32\ld_opt_hack.txt']
    else:
        env['CXXLINKFLAGS'] += ['-lncurses', '-lpthread', 'linux/fb_icon.c']
    if mac:
        # -no_pie (no position-independent execution) fixes a warning
        env['CXXLINKFLAGS'] += [os.path.join(libpath, 'operatornew.o'), '-Wl,-no_pie']
        if macSDKpath:
            env['CXXLINKFLAGS'] += ["-isysroot", macSDKpath]  # "-static-libgcc", '-weak-lSystem']

    def compile_main_module(target, source, env):
        """
        This is the emitter for BASEXE when using linkgcc: it compiles sources if needed, where
        the first specified module is the main module (-m flag), and rest are regular modules.
        """
        def to_o((i, obj)):
            if str(obj).endswith('.bas'):
                if i == 0:
                    return env.BASMAINO (obj)
                else:
                    return env.BASO (obj)
            return obj
        return target, map(to_o, enumerate(source))

    basexe_gcc = Builder (action = '$CXX $CXXFLAGS -o $TARGET $SOURCES $CXXLINKFLAGS',
                  suffix = exe_suffix, src_suffix = '.bas', emitter = compile_main_module)

    env['BUILDERS']['BASEXE'] = basexe_gcc


# Make a base environment for Game and Custom (other utilities use env)
commonenv = env.Clone ()

base_modules = []   # modules (any language) shared by all utilities (except bam2mid)
shared_modules = []  # FB/RB modules shared by, but with separate builds, for Game and Custom 
common_modules = []  # other modules (in any language) shared by Game and Custom
common_objects = []  # other objects shared by Game and Custom

libraries = []
libpaths = []

### gfx and music backend dependencies

gfx_map = {'fb': {'shared_modules': 'gfx_fb.bas', 'libraries': 'fbgfx fbmt'},
           'alleg' : {'shared_modules': 'gfx_alleg.bas', 'libraries': 'alleg'},
           'sdl' : {'shared_modules': 'gfx_sdl.bas', 'libraries': 'SDL'},
           'console' : {'shared_modules': 'gfx_console.bas', 'common_modules': 'curses_wrap.c'}, # probably also need to link pdcurses on windows, untested
           'directx' : {}, # nothing needed
           'sdlpp': {}     # nothing needed
           }

music_map = {'native':
                 {'shared_modules': 'music_native.bas music_audiere.bas',
                  'common_modules': os.path.join ('audwrap','audwrap.cpp'),
                  'libraries': 'audiere', 'libpaths': '.'},
             'native2':
                 {'shared_modules': 'music_native2.bas music_audiere.bas',
                  'common_modules': os.path.join ('audwrap','audwrap.cpp'),
                  'libraries': 'audiere', 'libpaths': '.'},
             'sdl':
                 {'shared_modules': 'music_sdl.bas sdl_lumprwops.bas',
                  'libraries': 'SDL SDL_mixer'},
             'silence':
                 {'shared_modules': 'music_silence.bas'}
            }

for k in gfx:
    for k2, v2 in gfx_map[k].items ():
        globals()[k2] += v2.split (' ')

for k in music:
    for k2, v2 in music_map[k].items ():
        globals()[k2] += v2.split (' ')

if win32:
    base_modules += ['os_windows.bas', 'os_windows2.c']
    libraries += ['fbgfx']
    libpaths += ['win32']
    commonenv['FBFLAGS'] += ['-s','gui']
elif mac:
    base_modules += ['os_unix.c']
    libraries += ['Cocoa']  # For CoreServices
    if 'sdl' in gfx:
        common_modules += ['mac/SDLmain.m']
        commonenv['FBFLAGS'] += ['-entry', 'SDL_main']
        if env.WhereIs('sdl-config'):
            commonenv['CFLAGS'] += [get_run_command("sdl-config --cflags").split()]
        else:
            commonenv['CFLAGS'] += ["-I", "/Library/Frameworks/SDL.framework/Headers", "-I", FRAMEWORKS_PATH + "/SDL.framework/Headers"]
elif unix:
    base_modules += ['os_unix.c']
    libraries += 'X11 Xext Xpm Xrandr Xrender pthread'.split (' ')
    commonenv['FBFLAGS'] += ['-d', 'DATAFILES=\'"/usr/share/games/ohrrpgce"\'']

#CXXLINKFLAGS are used when linking with g++
#FBLIBS are used when linking with fbc

for lib in libraries:
    if mac and lib in ('SDL', 'SDL_mixer', 'Cocoa'):
        # Use frameworks rather than normal unix libraries
        commonenv['CXXLINKFLAGS'] += ['-framework', lib]
        commonenv['FBLIBS'] += ['-Wl', '-framework,' + lib]
    else:
        commonenv['CXXLINKFLAGS'] += ['-l' + lib]
        commonenv['FBLIBS'] += ['-l', lib]

commonenv['CXXLINKFLAGS'] += ['-L' + path for path in libpaths]
commonenv['FBLIBS'] += Flatten ([['-p', v] for v in libpaths])


# first, make sure the version is saved.

# always do verprinting, before anything else.
verprint (gfx, music, 'svn', 'git', fbc)


base_modules += ['util.bas', 'blit.c', 'base64.c', 'unicode.c', 'array.c', 'vector.bas']

shared_modules += ['allmodex',
                   'backends',
                   'lumpfile',
                   'misc',
                   'bam2mid',
                   'common',
                   'bcommon',
                   'menus',
                   'browse',
                   'loading.rbas',
                   'reload',
                   'reloadext',
                   'sliceedit',
                   'slices']

edit_modules = ['custom',
                'customsubs',
                'drawing',
                'subs.rbas',
                'subs2',
                'subs4',
                'mapsubs',
                'flexmenu',
                'reloadedit',
                'editedit',
                'editrunner',
                'distribmenu']

game_modules = ['game',
                'bmod.rbas',
                'bmodsubs',
                'menustuf.rbas',
                'moresubs',
                'yetmore',
                'yetmore2',
                'savegame.rbas',
                'hsinterpreter']

common_modules += ['filelayer.cpp']

if 'raster' in ARGUMENTS:
    common_modules += ['rasterizer.cpp', 'matrixMath.cpp', 'gfx_newRenderPlan.cpp']
    commonenv['FBFLAGS'] += ['-d', 'USE_RASTERIZER']

if linkgcc:
    if win32:
        commonenv['CXXLINKFLAGS'] += ['-lgdi32', '-lwinmm', '-Wl,--subsystem,windows']

elif win32:
    if not os.path.isfile('libgcc_s.a'):
        shutil.copy(get_run_command("gcc -print-file-name=libgcc_s.a"), ".")
    if not os.path.isfile('libstdc++.a'):
        shutil.copy(get_run_command("gcc -print-file-name=libstdc++.a"), ".")
    commonenv['FBLIBS'] += ['-l','gcc_s','-l','stdc++']

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
    if linkgcc:
        # FIXME: This is a stopgap, it only works if the .rc files have previously been compiled
        gamesrc += [gameenv.RC('gicon.rc')]
        editsrc += [editenv.RC('cicon.rc')]
    else:
        gamesrc += ['gicon.rc']
        editsrc += ['cicon.rc']

def env_exe(name, **kwargs):
    ret = env.BASEXE (name, **kwargs)
    Alias (name, ret)
    return ret

GAME = gameenv.BASEXE   (gamename, source = gamesrc, FBFLAGS = gameflags)
CUSTOM = editenv.BASEXE (editname, source = editsrc, FBFLAGS = editflags)
env_exe ('bam2mid')
env_exe ('unlump', source = ['unlump.bas', 'lumpfile.o'] + base_objects)
env_exe ('relump', source = ['relump.bas', 'lumpfile.o'] + base_objects)
env_exe ('dumpohrkey', source = ['dumpohrkey.bas'] + base_objects)
env.Command ('hspeak', source = ['hspeak.exw', 'hsspiffy.e'], action = 'euc -gcc hspeak.exw -verbose')
RELOADTEST = env_exe ('reloadtest', source = ['reloadtest.bas'] + reload_objects)
XML2RELOAD = env_exe ('xml2reload', source = ['xml2reload.bas'] + reload_objects, FBLIBS = env['FBLIBS'] + ['-p','.', '-l','xml2'], CXXLINKFLAGS = env['CXXLINKFLAGS'] + ['-lxml2'])
RELOAD2XML = env_exe ('reload2xml', source = ['reload2xml.bas'] + reload_objects)
RELOADUTIL = env_exe ('reloadutil', source = ['reloadutil.bas'] + reload_objects)
RBTEST = env_exe ('rbtest', source = [env.RB('rbtest.rbas'), env.RB('rbtest2.rbas')] + reload_objects)
env_exe ('vectortest', source = ['vectortest.bas'] + base_objects)

# Building gfx_directx.dll
if win32:
    directx_sources = ['d3d.cpp', 'didf.cpp', 'gfx_directx.cpp', 'joystick.cpp', 'keyboard.cpp',
                       'midsurface.cpp', 'mouse.cpp', 'window.cpp']
    directx_sources = [os.path.join('gfx_directx', f) for f in directx_sources]

    RESFILE = w32_env.RES ('gfx_directx/gfx_directx.res', source = 'gfx_directx/gfx_directx.rc')
    Depends (RESFILE, ['gfx_directx/help.txt', 'gfx_directx/Ohrrpgce.bmp'])
    directx_sources.append (RESFILE)
    
    # Enable exceptions, most warnings, unicode
    w32_env.Append (CPPFLAGS = ['/EHsc', '/W3'], CPPDEFINES = ['UNICODE', '_UNICODE'])

    if int (ARGUMENTS.get ('debug', False)):
        # debug info, runtime error checking, static link debugging VC9.0 runtime lib, no optimisation
        w32_env.Append (CPPFLAGS = ['/Zi', '/RTC1', '/MTd', '/Od'], LINKFLAGS = ['/DEBUG'])
    else:
        # static link VC9.0 runtime lib, optimise, whole-program optimisation
        w32_env.Append (CPPFLAGS = ['/MT', '/O2', '/GL'], LINKFLAGS = ['/LTCG'])
    
    w32_env.SharedLibrary ('gfx_directx.dll', source = directx_sources,
                          LIBS = ['user32', 'ole32', 'gdi32'])
    TEST = w32_env.Program ('gfx_directx_test1.exe', source = ['gfx_directx/gfx_directx_test1.cpp'],
                            LIBS = ['user32'])
    Alias ('gfx_directx_test', TEST)


# --log . to smooth out inconsistencies between Windows and Unix
tmp = ''
if 'fb' in gfx:
    # Use gfx_fb because it draws far less frames without speed control for some reason, runs waaaay faster
    tmp = ' --gfx fb'
AUTOTEST = env.Command ('autotest_rpg', source = GAME, action =
                        [File(gamename).abspath + tmp +  ' --log . --runfast testgame/autotest.rpg',
                         'grep -q "TRACE: TESTS SUCCEEDED" g_debug.txt'])
INTERTEST = env.Command ('interactivetest', source = GAME, action =
                         [File(gamename).abspath + tmp + ' --log . --runfast testgame/interactivetest.rpg'
                          ' --replayinput testgame/interactivetest.ohrkey',
                          'grep -q "TRACE: TESTS SUCCEEDED" g_debug.txt'])

testprogs = ['reloadtest', 'rbtest', 'vectortest']
tests = [File(prog).abspath for prog in testprogs]
# The has to be some better way to do this...
env.Command ('test', source = testprogs + [XML2RELOAD, AUTOTEST, INTERTEST], action = tests)

Default (GAME)
Default (CUSTOM)

Alias ('game', GAME)
Alias ('custom', CUSTOM)
Alias ('reload', [RELOADUTIL, RELOAD2XML, XML2RELOAD, RELOADTEST, RBTEST])

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
                      Current (default) value: """ + "+".join (music) + """
  debug=0|1           Debugging builds:
                      Default: with or without -exx (FB error checking) depending
                               on platform, with C/C++ optimisation
                      debug=0: without -exx, strip executable.
                      debug=1: with -exx and without C/C++ optimisation
                      In all cases, compile with -g
  valgrind=1          valgrinding build.
  profile=1           Profiling build for gprof.
  scriptprofile=1     Script profiling build.
  linkgcc=0           Link using fbc instead of g++.
  asm=1               Produce .asm files instead of compiling.
  fbc=PATH            Override fbc.
  macsdk=version      Target a previous version of Mac OS X, eg. 10.4
                      You will need the relevant SDK installed, and need to use a
                      copy of FB built against that SDK.

Experimental options:
  raster=1            Include new graphics API and rasterizer.
  gengcc=1            Compile using GCC emitter.
  deprecated=1        Compiles certain source files using the "deprecated" dialect

Targets:
  """ + gamename + """ (or game)
  """ + editname + """ (or custom)
  gfx_directx.dll
  unlump
  relump
  hspeak
  reloadtest
  xml2reload
  reload2xml
  reloadutil
  vectortest
  rbtest
  gfx_directx_test    (Non-automated) gfx_directx.dll test
  dumpohrkey
  bam2mid
  reload              Compile all RELOAD utilities.
  autotest_rpg        Runs autotest.rpg. See autotest.py for improved harness.
  interactivetest     Runs interactivetest.rpg with recorded input.
  test                Compile and run all automated tests, including autotest.rpg.
  .                   Compile everything (and run tests).

With no targets specified, compiles game and custom.

Examples:
  scons
  scons gfx=sdl+fb music=native game custom
  scons -j 2 debug=1 .
""")
