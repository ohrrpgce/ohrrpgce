#!/usr/bin/env python
"""Main scons build script for OHRRPGCE

cf. SConstruct, ohrbuild.py
"""
import os
import platform
import shutil
import shlex
import itertools
import re
from ohrbuild import basfile_scan, verprint, android_source_files, get_run_command

FBFLAGS = ['-mt']
if 'FBFLAGS' in os.environ:
    FBFLAGS += shlex.split (os.environ['FBFLAGS'])
#CC and CXX are probably not needed anymore
CC = os.environ.get ('CC')
CXX = os.environ.get ('CXX')
AS = os.environ.get ('AS')
fbc = ARGUMENTS.get ('fbc','fbc')
fbc = os.path.expanduser (fbc)  # expand ~
# Use gnu99 dialect instead of c99. c99 causes GCC to define __STRICT_ANSI__
# which causes types like off_t and off64_t to be renamed to _off_t and _off64_t
# under MinGW. (See bug 951)
CFLAGS = '-g -Wall --std=gnu99'.split ()  # These flags apply only to .c[pp] sources, NOT to CC invoked via gengcc=1
CXXFLAGS = '-g -Wall -Wno-non-virtual-dtor'.split ()
linkgcc = int (ARGUMENTS.get ('linkgcc', True))   # link using g++ instead of fbc?
envextra = {}
FRAMEWORKS_PATH = "~/Library/Frameworks"  # Frameworks search path in addition to the default /Library/Frameworks

win32 = False
unix = False
mac = False
android = False
android_source = False
arch = 'x86'
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

if 'android-source' in ARGUMENTS:
    # Produce .c files, and also an executable, which is an unwanted side product
    # (We could do with build targets for compiling to .asm/.c but not assembling+linking)
    FBFLAGS += ["-r"]
    android = True
    android_source = True
    linkgcc = False
elif 'android' in ARGUMENTS:
    android = True

if android:
    FBFLAGS += ["-d", "__FB_ANDROID__=1"]
    arch = 'armeabi'

if arch == 'armeabi':
    FBFLAGS += ["-gen", "gcc", "-arch", "arm", "-R"]
    #CFLAGS += -L$(SYSROOT)/usr/lib
    # CC, CXX, AS must be set in environment to point to cross compiler
elif arch == 'x86':
    CFLAGS.append ('-m32')
    CXXFLAGS.append ('-m32')
    # Recent versions of GCC default to assuming the stack is kept 16-byte aligned
    # (which is a recent change in the Linux x86 ABI) but fbc's GAS backend is not yet updated for that
    CFLAGS.append ('-mpreferred-stack-boundary=2')
    CXXFLAGS.append ('-mpreferred-stack-boundary=2')
    # gcc -m32 on x86_64 defaults to enabling SSE and SSE2, so disable that,
    # except on Intel Macs, where it is both always present, and required by system headers
    if not mac:
        CFLAGS.append ('-mno-sse')
        CXXFLAGS.append ('-mno-sse')
    #FBFLAGS += ["-arch", "686"]
elif arch == 'x86_64':
    CFLAGS.append ('-m64')
    CXXFLAGS.append ('-m64')
    # This also causes FB to default to -gen gcc, as -gen gas not supported
    # (therefore we don't need to pass -mpreferred-stack-boundary=2)
    FBFLAGS += ['-arch', 'x86_64']
else:
    raise Exception('Unknown architecture %s' % arch)

if 'asm' in ARGUMENTS:
    FBFLAGS += ["-r", "-g"]

# There are three levels of debug here. Not specifying 'debug' is a happy medium
debug = 0.5
if 'debug' in ARGUMENTS:
    debug = float (ARGUMENTS['debug'])
optimisations = (debug <= 0.5)    # compile with compiler optimisations?
FB_exx = (debug >= 0.5)   # compile with -exx?
FB_g = (debug >= 0.5)   # compile with -g?
GCC_strip = (debug == 0)  # (linkgcc only) strip (link with -s)?

profile = int (ARGUMENTS.get ('profile', 0))
if profile:
    FBFLAGS.append ('-profile')
    CFLAGS.append ('-pg')
    CXXFLAGS.append ('-pg')
    FB_g = True
    GCC_strip = False
if int (ARGUMENTS.get ('scriptprofile', 0)):
    FBFLAGS += ['-d','SCRIPTPROFILE']
if int (ARGUMENTS.get ('valgrind', 0)):
    #-exx under valgrind is nearly redundant, and really slow
    FB_exx = False
    CFLAGS.append ('-DVALGRIND_ARRAYS')
if FB_exx:
    FBFLAGS.append ('-exx')
if FB_g:
    FBFLAGS.append ('-g')
if optimisations:
    CFLAGS.append ('-O3')
    CXXFLAGS.append ('-O3')
    FBFLAGS += ["-O", "2"]
if int (ARGUMENTS.get ('gengcc', 0)):
    # Due to FB bug #661, need to pass -m32 to gcc manually (although recent versions of FB do pass it?)
    if profile or debug >= 0.5:
        # -O2 plus profiling crashes for me due to mandatory frame pointers being omitted.
        # Also keep frame pointers unless explicit debug=0
        FBFLAGS += ["-gen", "gcc", "-Wc", "-m32,-fno-omit-frame-pointer"]
    else:
        FBFLAGS += ["-gen", "gcc", "-Wc", "-m32"]

# Backend selection.
if mac:
    gfx = 'sdl'
elif android:
    gfx = 'sdl'
elif unix:
    gfx = 'sdl+fb'
elif win32:
    gfx = 'directx+sdl+fb'
gfx = ARGUMENTS.get ('gfx', os.environ.get ('OHRGFX', gfx))
gfx = gfx.split ("+")
gfx = [g.lower () for g in gfx]
music = ARGUMENTS.get ('music', os.environ.get ('OHRMUSIC','sdl'))
music = [music.lower ()]

env = Environment (FBFLAGS = FBFLAGS,
                   FBLINKFLAGS = [],
                   CFLAGS = CFLAGS,
                   FBC = fbc,
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
    env['FBLINKFLAGS'] += ['-p', 'win32']
    env['CXXLINKFLAGS'] += ['-L', 'win32']

    w32_env = Environment ()
    if 'DXSDK_DIR' in os.environ:
        w32_env.Append(CPPPATH = os.path.join(os.environ['DXSDK_DIR'], 'Include'))
        w32_env.Append(LIBPATH = os.path.join(os.environ['DXSDK_DIR'], 'Lib', 'x86'))

if mac:
    macsdk = ARGUMENTS.get ('macsdk', '')
    macSDKpath = ''
    env['FBLINKFLAGS'] += ['-Wl', '-F,' + FRAMEWORKS_PATH]
    env['CXXLINKFLAGS'] += ['-F', FRAMEWORKS_PATH]
    if macsdk:
        macSDKpath = 'MacOSX' + macsdk + '.sdk'
        if macsdk == '10.4':
            macSDKpath = 'MacOSX10.4u.sdk'
        macSDKpath = '/Developer/SDKs/' + macSDKpath
        if not os.path.isdir(macSDKpath):
            raise Exception('Mac SDK ' + macsdk + ' not installed: ' + macSDKpath + ' is missing')
        env['FBLINKFLAGS'] += ['-Wl', '-mmacosx-version-min=' + macsdk]
        env['CFLAGS'] += ['-mmacosx-version-min=' + macsdk]
        env['CXXFLAGS'] += ['-mmacosx-version-min=' + macsdk]

if android:
    # liblog for __android_log_print/write
    env['FBLINKFLAGS'] += ['-l', 'log']
    env['CXXLINKFLAGS'] += ['-llog']

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
basexe = Builder (action = '$FBC $FBFLAGS -x $TARGET $SOURCES $FBLINKFLAGS',
                  suffix = exe_suffix, src_suffix = '.bas')

basasm = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS -r -g',
                suffix = '.asm', src_suffix = '.bas', single_source = True)

# Surely there's a simpler way to do this
def depend_on_reloadbasic_py(target, source, env):
    return (target, source + ['reloadbasic/reloadbasic.py'])

rbasic_builder = Builder (action = [[File('reloadbasic/reloadbasic.py'), '--careful', '$SOURCE', '-o', '$TARGET']],
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

if AS:
    env['ENV']['AS'] = AS

if CC:
    env['ENV']['CC'] = CC
    env['ENV']['GCC'] = CC  # fbc only checks GCC variable, not CC
    env.Replace (CC = CC)

if CXX:
    env['ENV']['CXX'] = CXX
    env.Replace (CXX = CXX)

if linkgcc:
    fbc_binary = fbc
    if not os.path.isfile (fbc_binary):
        fbc_binary = env.WhereIs (fbc)
    if not fbc_binary:
        raise Exception("FreeBasic compiler is not installed!")
    fbc_path = os.path.dirname(fbc_binary)
    #print "fbc = " + fbc_path
    if win32:
        target = 'win32'
    elif android:
        if not CC or not CXX or not AS:
            raise Exception("You need to set CC, CXX, AS environmental variables correctly to crosscompile to Android")
        target = get_run_command(CC + " -dumpmachine")
        if target != 'arm-linux-androideabi':
            raise Exception("This GCC doesn't target arm-linux-androideabi. You need to set CC, CXX, AS environmental variables correctly to crosscompile to Android")
        target += '-freebasic'
    else:
        fbcinfo = get_run_command(fbc_binary + " -version")
        target = re.findall("target:([a-z]*)", fbcinfo)
        if len(target) == 0:
            target = re.findall("\) for ([a-z0-9]+)\n", fbcinfo)
            if len(target) == 0:
                raise Exception("Couldn't determine fbc target")
        target = target[0]

    fblibpaths = [[fbc_path, 'lib'],
                  [fbc_path, '..', 'lib'],
                  [fbc_path, '..', 'lib', 'freebasic'],
                  ['/usr/share/freebasic/lib'],
                  ['/usr/local/lib/freebasic']]
    # For each of the above possible library paths, check three possible target subdirectories:
    targetdirs = [ [], [arch + '-' + target], [target] ]
    # FB since 0.25 doesn't seem to use a platform subdirectory in lib ?

    for path, targetdir in itertools.product(fblibpaths, targetdirs):
        libpath = os.path.join(*(path + targetdir))
        #print "Looking for FB libs in", libpath
        if os.path.isfile(os.path.join(libpath, 'fbrt0.o')):
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
        # winmm needed for MIDI, used by music backends but also by miditest, so I'll include it here rather than in commonenv
        env['CXXLINKFLAGS'] += ['-lwinmm']
    else:
        if 'fb' in gfx:
            # Program icon required by fbgfx, but we only provide it on Windows,
            # because on X11 need to provide it as an XPM instead
            env['CXXLINKFLAGS'] += ['linux/fb_icon.c']
        # Android doesn't have ncurses, and libpthread is part of libc
        if not android:
            # The following are required by libfb
            env['CXXLINKFLAGS'] += ['-lncurses', '-lpthread']

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
    
    if mac:
        basexe_gcc_action = '$CXX $CXXFLAGS -o $TARGET $SOURCES $CXXLINKFLAGS'
    else:
        basexe_gcc_action = '$CXX $CXXFLAGS -o $TARGET $SOURCES "-Wl,-(" $CXXLINKFLAGS "-Wl,-)"'
    basexe_gcc = Builder (action = basexe_gcc_action, suffix = exe_suffix, src_suffix = '.bas', emitter = compile_main_module)

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
             'allegro':
                 {'shared_modules': 'music_allegro.bas',
                  'libraries': 'alleg'},
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
            commonenv.ParseConfig('sdl-config --cflags')
        else:
            commonenv['CFLAGS'] += ["-I", "/Library/Frameworks/SDL.framework/Headers", "-I", FRAMEWORKS_PATH + "/SDL.framework/Headers"]
elif android:
    base_modules += ['os_unix.c']
elif unix:
    base_modules += ['os_unix.c']
    if gfx != ['console']:
        # All graphical gfx backends need the X11 libs
        libraries += 'X11 Xext Xpm Xrandr Xrender'.split (' ')
    commonenv['FBFLAGS'] += ['-d', 'DATAFILES=\'"/usr/share/games/ohrrpgce"\'']

#CXXLINKFLAGS are used when linking with g++
#FBLINKFLAGS are used when linking with fbc

for lib in libraries:
    if mac and lib in ('SDL', 'SDL_mixer', 'Cocoa'):
        # Use frameworks rather than normal unix libraries
        commonenv['CXXLINKFLAGS'] += ['-framework', lib]
        commonenv['FBLINKFLAGS'] += ['-Wl', '-framework,' + lib]
    else:
        commonenv['CXXLINKFLAGS'] += ['-l' + lib]
        commonenv['FBLINKFLAGS'] += ['-l', lib]

commonenv['CXXLINKFLAGS'] += ['-L' + path for path in libpaths]
commonenv['FBLINKFLAGS'] += Flatten ([['-p', v] for v in libpaths])


# first, make sure the version is saved.

# always do verprinting, before anything else.
verprint (gfx, music, 'svn', 'git', fbc)


base_modules += ['util.bas', 'blit.c', 'base64.c', 'unicode.c', 'array.c', 'miscc.c', 'vector.bas']

shared_modules += ['allmodex',
                   'backends',
                   'lumpfile',
                   'misc',
                   'bam2mid',
                   'common.rbas',
                   'bcommon',
                   'menus',
                   'browse',
                   'loading.rbas',
                   'reload',
                   'reloadext',
                   'sliceedit',
                   'slices']

edit_modules = ['custom',
                'customsubs.rbas',
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
                'moresubs.rbas',
                'yetmore',
                'yetmore2',
                'savegame.rbas',
                'scripting',
                'oldhsinterpreter',
                'purchase.rbas']

common_modules += ['filelayer.cpp']

if 'raster' in ARGUMENTS:
    common_modules += ['rasterizer.cpp', 'matrixMath.cpp', 'gfx_newRenderPlan.cpp']
    commonenv['FBFLAGS'] += ['-d', 'USE_RASTERIZER']

if linkgcc:
    if win32:
        commonenv['CXXLINKFLAGS'] += ['-lgdi32', '-Wl,--subsystem,windows']

else:
    commonenv['FBLINKFLAGS'] += ['-l','stdc++'] #, '-l','gcc_s', '-l','gcc_eh']

# Note that base_objects are not built in commonenv!
base_objects = sum ([env.Object(a) for a in base_modules], [])  # concatenate lists
common_objects += base_objects + sum ([commonenv.Object(a) for a in common_modules], [])
# Plus unique module included by utilities but not Game or Custom
base_objects.extend (env.Object ('common_base.bas'))

gameenv = commonenv.Clone (VAR_PREFIX = 'game-', FBFLAGS = commonenv['FBFLAGS'] + \
                      ['-d','IS_GAME', '-m','game'])
editenv = commonenv.Clone (VAR_PREFIX = 'edit-', FBFLAGS = commonenv['FBFLAGS'] + \
                      ['-d','IS_CUSTOM', '-m','custom'])

#now... GAME and CUSTOM

gamesrc = common_objects[:]
for item in game_modules:
    gamesrc.extend (gameenv.BASO (item))
for item in shared_modules:
    gamesrc.extend (gameenv.VARIANT_BASO (item))

editsrc = common_objects[:]
for item in edit_modules:
    editsrc.extend (editenv.BASO (item))
for item in shared_modules:
    editsrc.extend (editenv.VARIANT_BASO (item))

# For reload utilities
reload_objects = base_objects + sum ([env.BASO (item) for item in ['reload', 'reloadext', 'lumpfile']], [])

base_objects_without_util = [a for a in base_objects if str(a) != 'util.o']

gamename = 'ohrrpgce-game'
editname = 'ohrrpgce-custom'
gameflags = list (gameenv['FBFLAGS']) #+ ['-v']
editflags = list (editenv['FBFLAGS']) #+ ['-v']

if win32:
    gamename = 'game'
    editname = 'custom'
    if linkgcc:
        # FIXME: This is a stopgap, it only works if the .rc files have previously been compiled
        gamesrc += gameenv.RC('gicon.rc')
        editsrc += editenv.RC('cicon.rc')
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
env_exe ('miditest')
env_exe ('unlump', source = ['unlump.bas', 'lumpfile.o'] + base_objects)
env_exe ('relump', source = ['relump.bas', 'lumpfile.o'] + base_objects)
env_exe ('dumpohrkey', source = ['dumpohrkey.bas'] + base_objects)
env.Command ('hspeak', source = ['hspeak.exw', 'hsspiffy.e'], action = 'euc -gcc hspeak.exw -verbose')
RELOADTEST = env_exe ('reloadtest', source = ['reloadtest.bas'] + reload_objects)
x2rsrc = ['xml2reload.bas'] + reload_objects
if win32:
    # Hack around our provided libxml2.a lacking a function. (Was less work than recompiling)
    x2rsrc.append (env.Object('win32/utf8toisolat1.c'))
XML2RELOAD = env_exe ('xml2reload', source = x2rsrc, FBLINKFLAGS = env['FBLINKFLAGS'] + ['-l','xml2'], CXXLINKFLAGS = env['CXXLINKFLAGS'] + ['-lxml2'])
RELOAD2XML = env_exe ('reload2xml', source = ['reload2xml.bas'] + reload_objects)
RELOADUTIL = env_exe ('reloadutil', source = ['reloadutil.bas'] + reload_objects)
RBTEST = env_exe ('rbtest', source = [env.RB('rbtest.rbas'), env.RB('rbtest2.rbas')] + reload_objects)
env_exe ('vectortest', source = ['vectortest.bas'] + base_objects)
# Compile util.bas as a main module to utiltest.o to prevent its linkage in other binaries
env_exe ('utiltest', source = env.BASMAINO('utiltest.o', 'util.bas') + base_objects_without_util)

if android_source:
    # This is hacky and will be totally rewritten
    if 'game' in COMMAND_LINE_TARGETS:
        android_source_files (gamesrc)
    elif 'custom' in COMMAND_LINE_TARGETS:
        android_source_files (editsrc)
    else:
        raise Exception("Specify either 'game' or 'custom' as a target with android-source=1")

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

    if optimisations == False:
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
# This prevents more than one copy of Game from being run at once
# (doesn't matter where g_debug.txt is actually placed)
SideEffect ('g_debug.txt', [AUTOTEST, INTERTEST])

testprogs = ['reloadtest', 'rbtest', 'vectortest', 'utiltest']
tests = [File(prog).abspath for prog in testprogs]
# There has to be some better way to do this...
TESTS = env.Command ('test', source = testprogs + [AUTOTEST, INTERTEST], action = tests)
Alias ('tests', TESTS)

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
  debug=0|0.5|1       Debugging builds:
                      debug=0:   without -exx, with C/C++ optimisation, strip executable.
                      debug=0.5: with -exx (FB error checking), debug symbols, and
                       (default)   C/C++ optimisation
                      debug=1:   with -exx and without C/C++ optimisation
  valgrind=1          valgrinding build.
  profile=1           Profiling build for gprof.
  scriptprofile=1     Script profiling build.
  asm=1               Produce .asm or .c files instead of compiling
                      (Still tries to assemble and link, ignore)
  fbc=PATH            Override fbc.
  macsdk=version      Target a previous version of Mac OS X, eg. 10.4
                      You will need the relevant SDK installed, and need to use a
                      copy of FB built against that SDK.

Experimental options:
  raster=1            Include new graphics API and rasterizer.
  gengcc=1            Compile using GCC emitter.
  linkgcc=0           Link using fbc instead of g++.
  android=1           Compile for android. Commandline programs only.
  android-source=1    Used as part of the Android build process for Game/Custom.

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
  utiltest
  vectortest
  rbtest
  gfx_directx_test    (Non-automated) gfx_directx.dll test
  dumpohrkey
  bam2mid
  miditest
  reload              Compile all RELOAD utilities.
  autotest_rpg        Runs autotest.rpg. See autotest.py for improved harness.
  interactivetest     Runs interactivetest.rpg with recorded input.
  test (or tests)     Compile and run all automated tests, including autotest.rpg.
  .                   Compile everything (and run tests).

With no targets specified, compiles game and custom.

Examples:
  scons
  scons gfx=sdl+fb music=native game custom
  scons -j 2 debug=1 .
""")
