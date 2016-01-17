#!/usr/bin/env python
"""Main scons build script for OHRRPGCE
Run "scons -h" to print help.

cf. SConstruct, ohrbuild.py
"""
import sys
import os
import platform
import shutil
import shlex
import itertools
import re
from ohrbuild import basfile_scan, verprint, android_source_actions, get_command_output

FBFLAGS = ['-mt']
verbose = int (ARGUMENTS.get ('v', False))
if verbose:
    FBFLAGS += ['-v']
if 'FBFLAGS' in os.environ:
    FBFLAGS += shlex.split (os.environ['FBFLAGS'])
fbc = ARGUMENTS.get ('fbc','fbc')
fbc = os.path.expanduser (fbc)  # expand ~
# Use gnu99 dialect instead of c99. c99 causes GCC to define __STRICT_ANSI__
# which causes types like off_t and off64_t to be renamed to _off_t and _off64_t
# under MinGW. (See bug 951)
CFLAGS = '-g -Wall --std=gnu99'.split ()  # These flags apply only to .c[pp] sources, NOT to CC invoked via gengcc=1
CXXFLAGS = '-g -Wall -Wno-non-virtual-dtor'.split ()
CXXLINKFLAGS = []
linkgcc = int (ARGUMENTS.get ('linkgcc', True))   # link using g++ instead of fbc?
envextra = {}
FRAMEWORKS_PATH = "~/Library/Frameworks"  # Frameworks search path in addition to the default /Library/Frameworks
destdir = ARGUMENTS.get ('destdir', '')
prefix =  ARGUMENTS.get ('prefix', '/usr')

base_libraries = []  # libraries shared by all utilities (except bam2mid)

win32 = False
unix = False
mac = False
android = False
android_source = False
arch = ARGUMENTS.get ('arch', 'x86')
if arch == '32':
    arch = 'x86'
if arch in ('x64', '64'):
    arch = 'x86_64'
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

if int (ARGUMENTS.get ('asm', False)):
    FBFLAGS += ["-R", "-RR", "-g"]

if int (ARGUMENTS.get ('glibc', False)):
    # No need to bother automatically checking for glibc
    CFLAGS += ["-DHAVE_GLIBC"]

# There are four levels of debug here: 0, 1, 2, 3. See the help.
debug = 2  # Default to happy medium
if 'debug' in ARGUMENTS:
    debug = int (ARGUMENTS['debug'])
optimisations = (debug < 3)    # compile with C/C++/FB optimisations?
FB_exx = (debug in (2,3))     # compile with -exx?
FB_g = (debug >= 1)       # compile with -g?
# Note: fbc includes symbols (but not debug info) in .o files even without -g,
# but strips everything if -g not passed during linking; with linkgcc we need to strip.
GCC_strip = (debug == 0)  # (linkgcc only) strip debug info?

gengcc = int (ARGUMENTS.get ('gengcc', 0))
profile = int (ARGUMENTS.get ('profile', 0))
if profile:
    FBFLAGS.append ('-profile')
    CFLAGS.append ('-pg')
    CXXFLAGS.append ('-pg')
if int (ARGUMENTS.get ('scriptprofile', 0)):
    FBFLAGS += ['-d','SCRIPTPROFILE']
if int (ARGUMENTS.get ('valgrind', 0)):
    #-exx under valgrind is nearly redundant, and really slow
    FB_exx = False
    # This changes memory layout of vectors to be friendlier to valgrind
    CFLAGS.append ('-DVALGRIND_ARRAYS')
asan = int (ARGUMENTS.get ('asan', 0))
if asan:
    # AddressSanitizer is supported by both gcc & clang. They are responsible for linking runtime library
    assert linkgcc, "linkgcc=0 asan=1 combination not supported."
    CFLAGS.append ('-fsanitize=address')
    CXXFLAGS.append ('-fsanitize=address')
    CXXLINKFLAGS.append ('-fsanitize=address')
    base_libraries.append ('dl')
    # Also, compile FB to C by default, unless overridden with gengcc=0.
    if int (ARGUMENTS.get ('gengcc', 1)):
        gengcc = True
        FB_exx = False  # Superceded by AddressSanitizer
if FB_exx:
    FBFLAGS.append ('-exx')
if FB_g:
    FBFLAGS.append ('-g')
if optimisations:
    CFLAGS.append ('-O3')
    CXXFLAGS.append ('-O3')
    # FB optimisation flag currently does pretty much nothing unless using -gen gcc
    FBFLAGS += ["-O", "2"]
if gengcc:
    FBFLAGS += ["-gen", "gcc"]

# Backend selection.
# Defaults:
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


################ Create base environment

#CXXLINKFLAGS are used when linking with g++
#FBLINKFLAGS are used when linking with fbc

env = Environment (FBFLAGS = FBFLAGS,
                   FBLINKFLAGS = [],
                   CFLAGS = CFLAGS,
                   FBC = fbc,
                   CXXFLAGS = CXXFLAGS,
                   CXXLINKFLAGS = CXXLINKFLAGS,
                   VAR_PREFIX = '',
                   **envextra)
# These no longer do anything
del CFLAGS
del CXXFLAGS
del FBFLAGS
del CXXLINKFLAGS

# Shocked that scons doesn't provide $HOME
# $DISPLAY is need for both gfx_sdl and gfx_fb (when running tests)
for var in 'PATH', 'DISPLAY', 'HOME', 'EUDIR', 'AS', 'CC', 'CXX':
    if var in os.environ:
        env['ENV'][var] = os.environ[var]

#CC and CXX are probably not needed anymore
AS = os.environ.get ('AS')
CC = os.environ.get ('CC')
CXX = os.environ.get ('CXX')
clang = False
if CC:
    clang = 'clang' in CC
    if not clang:
        # fbc does not support -gen gcc using clang
        env['ENV']['GCC'] = CC  # fbc only checks GCC variable, not CC
    env.Replace (CC = CC)
if CXX:
    env.Replace (CXX = CXX)


################ Define Builders and Scanners for FreeBASIC and ReloadBasic

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
                  suffix = exe_suffix, src_suffix = '.bas', source_factory = translate_rb)

# Not used; use asm=1
basasm = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS -r -g',
                suffix = '.asm', src_suffix = '.bas', single_source = True,
                emitter = prefix_targets, source_factory = translate_rb)

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


################ Find fbc and get fbcinfo fbcversion

fbc_binary = fbc
if not os.path.isfile (fbc_binary):
    fbc_binary = env.WhereIs (fbc)
if not fbc_binary:
    raise Exception("FreeBasic compiler is not installed!")
fbc_path = os.path.dirname(os.path.realpath(fbc_binary))
# Newer versions of fbc (1.0+) print e.g. "FreeBASIC Compiler - Version $VER ($DATECODE), built for linux-x86 (32bit)"
# older versions printed "FreeBASIC Compiler - Version $VER ($DATECODE) for linux"
# older still printed "FreeBASIC Compiler - Version $VER ($DATECODE) for linux (target:linux)"
fbcinfo = get_command_output(fbc_binary, "-version")
fbcversion = re.findall("Version ([0-9.]*)", fbcinfo)[0]
# Convert e.g. 1.04.1 into 1041
fbcversion = int(fbcversion.replace('.', ''))
if verbose:
    print "Using fbc", fbc_binary, " version:", fbcversion, " arch:", arch

# FB 0.91 added a multithreaded version of libfbgfx
if fbcversion >= 910:
    libfbgfx = 'fbgfxmt'
else:
    libfbgfx = 'fbgfx'


################ Mac SDKs

if mac:
    macsdk = ARGUMENTS.get ('macsdk', '')
    macSDKpath = ''
    env['FBLINKFLAGS'] += ['-Wl', '-F,' + FRAMEWORKS_PATH]
    env['CXXLINKFLAGS'] += ['-F', FRAMEWORKS_PATH]
    if macsdk:
        if macsdk == '10.4':
            # 10.4 has a different naming scheme
            macSDKpath = 'MacOSX10.4u.sdk'
        else:
            macSDKpath = 'MacOSX' + macsdk + '.sdk'
        macSDKpath = '/Developer/SDKs/' + macSDKpath
        if not os.path.isdir(macSDKpath):
            raise Exception('Mac SDK ' + macsdk + ' not installed: ' + macSDKpath + ' is missing')
        env['FBLINKFLAGS'] += ['-Wl', '-mmacosx-version-min=' + macsdk]
        env['CFLAGS'] += ['-mmacosx-version-min=' + macsdk]
        env['CXXFLAGS'] += ['-mmacosx-version-min=' + macsdk]


################ Arch-specific stuff

if arch == 'armeabi':
    gengcc = True
    env['FBFLAGS'] += ["-gen", "gcc", "-arch", "arm", "-R"]
    #env['CFLAGS'] += -L$(SYSROOT)/usr/lib
    # CC, CXX, AS must be set in environment to point to cross compiler
elif arch == 'x86':
    env['CFLAGS'].append ('-m32')
    env['CXXFLAGS'].append ('-m32')
    if not clang:
        # Recent versions of GCC default to assuming the stack is kept 16-byte aligned
        # (which is a recent change in the Linux x86 ABI) but fbc's GAS backend is not yet updated for that
        # I don't know what clang does, but it doesn't support this commandline option.
        env['CFLAGS'].append ('-mpreferred-stack-boundary=2')
        env['CXXFLAGS'].append ('-mpreferred-stack-boundary=2')
    # gcc -m32 on x86_64 defaults to enabling SSE and SSE2, so disable that,
    # except on Intel Macs, where it is both always present, and required by system headers
    if not mac:
        env['CFLAGS'].append ('-mno-sse')
        env['CXXFLAGS'].append ('-mno-sse')
    #env['FBFLAGS'] += ["-arch", "686"]
elif arch == 'x86_64':
    gengcc = True
    env['CFLAGS'].append ('-m64')
    env['CXXFLAGS'].append ('-m64')
    # This also causes FB to default to -gen gcc, as -gen gas not supported
    # (therefore we don't need to pass -mpreferred-stack-boundary=2)
    env['FBFLAGS'] += ['-arch', 'x86_64']
else:
    raise Exception('Unknown architecture %s' % arch)

if gengcc:
    # NOTE: You can only pass -Wc (which passes flags on to gcc) once to fbc; the last -Wc overrides others!
    gcc_flags = []
    if FB_exx:
        # -exx results in a lot of labelled goto use, which confuses gcc
        gcc_flags.append ('-Wno-maybe-uninitialized')
    if profile or debug >= 1:
        # -O2 plus profiling crashes for me due to mandatory frame pointers being omitted.
        # Also keep frame pointers unless explicit debug=0
        gcc_flags.append ('-fno-omit-frame-pointer')
    if asan:
        # Use AddressSanitizer in C files produced by fbc
        gcc_flags.append ('-fsanitize=address')
    if len(gcc_flags):
        env['FBFLAGS'] += ["-Wc", ','.join (gcc_flags)]


################ A bunch of stuff for linking

if linkgcc:
    # Link using g++ instead of fbc; this makes it easy to link correct C++ libraries, but harder to link FB

    # target should be the OS code, with the arch?
    # Looks like the code below doesn't care, as long as it finds the right directory
    if win32:
        target = 'win32'
    elif android:
        if not CC or not CXX or not AS:
            raise Exception("You need to set CC, CXX, AS environmental variables correctly to crosscompile to Android")
        target = get_command_output(CC, "-dumpmachine")
        if target != 'arm-linux-androideabi':
            raise Exception("This GCC doesn't target arm-linux-androideabi. You need to set CC, CXX, AS environmental variables correctly to crosscompile to Android")
        target += '-freebasic'
    else:
        target = re.findall("target:([a-z]*)", fbcinfo)
        if len(target) == 0:
            # Omit the arch
            target = re.findall(" for ([a-z0-9]+)", fbcinfo)
            if len(target) == 0:
                raise Exception("Couldn't determine fbc target")
                # Or just default to the current platform
        target = target[0]
    if verbose:
        print "linkgcc: target =", target

    fblibpaths = [[fbc_path, 'lib'],
                  [fbc_path, '..', 'lib'],
                  [fbc_path, '..', 'lib', 'freebasic'],
                  ['/usr/share/freebasic/lib'],
                  ['/usr/local/lib/freebasic']]
    # For each of the above possible library paths, check four possible target subdirectories:
    # (FB changes where the libraries are stored every other month)
    targetdirs = [ [], [target + '-' + arch], [arch + '-' + target], [target] ]
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
    if verbose:
        env['CXXLINKFLAGS'] += ['-v']
    if GCC_strip:
        # Strip debug info but leave in the function (and unwanted global) symbols.
        env['CXXLINKFLAGS'] += ['-Wl,-S']
    if win32:
        # win32\ld_opt_hack.txt contains --stack option which can't be passed using -Wl
        env['CXXLINKFLAGS'] += ['-static-libgcc', '-static-libstdc++', '-Wl,@win32\ld_opt_hack.txt']
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

if not linkgcc:
    # At the moment we don't link C++ into any utilities, so this is actually only needed in commonenv
    env['FBLINKFLAGS'] += ['-l','stdc++'] #, '-l','gcc_s', '-l','gcc_eh']


################ Program-specific stuff starts here

# We have five environments:
# env             : Used for utilities
# +-> commonenv   : Not used directly, but common configuration shared by gameenv and editenv
#     +-> gameenv : For Game
#     +-> editenv : For Custom
# w32_env         : For gfx_directx (completely separate; uses Visual C++)

# Make a base environment for Game and Custom (other utilities use env)
commonenv = env.Clone ()

# Added to env and commonenv
base_modules = []   # modules (any language) shared by all executables (except bam2mid)
#base_libraries defined above

# Added to gameenv and editenv
shared_modules = []  # FB/RB modules shared by, but with separate builds, for Game and Custom

# Added to commonenv
common_modules = []  # other modules (in any language) shared by Game and Custom; only built once
common_libraries = []
common_libpaths = []


################ gfx and music backend modules and libraries

gfx_map = {'fb': {'shared_modules': 'gfx_fb.bas', 'common_libraries': libfbgfx},
           'alleg' : {'shared_modules': 'gfx_alleg.bas', 'common_libraries': 'alleg'},
           'sdl' : {'shared_modules': 'gfx_sdl.bas', 'common_libraries': 'SDL'},
           'console' : {'shared_modules': 'gfx_console.bas', 'common_modules': 'curses_wrap.c'}, # probably also need to link pdcurses on windows, untested
           'directx' : {}, # nothing needed
           'sdlpp': {}     # nothing needed
           }

music_map = {'native':
                 {'shared_modules': 'music_native.bas music_audiere.bas',
                  'common_modules': os.path.join ('audwrap','audwrap.cpp'),
                  'common_libraries': 'audiere', 'common_libpaths': '.'},
             'native2':
                 {'shared_modules': 'music_native2.bas music_audiere.bas',
                  'common_modules': os.path.join ('audwrap','audwrap.cpp'),
                  'common_libraries': 'audiere', 'common_libpaths': '.'},
             'sdl':
                 {'shared_modules': 'music_sdl.bas sdl_lumprwops.bas',
                  'common_libraries': 'SDL SDL_mixer'},
             'allegro':
                 {'shared_modules': 'music_allegro.bas',
                  'common_libraries': 'alleg'},
             'silence':
                 {'shared_modules': 'music_silence.bas'}
            }

for k in gfx:
    for k2, v2 in gfx_map[k].items ():
        globals()[k2] += v2.split (' ')

for k in music:
    for k2, v2 in music_map[k].items ():
        globals()[k2] += v2.split (' ')


################ OS-specific modules and libraries

if win32:
    base_modules += ['os_windows.bas', 'os_windows2.c']
    # winmm needed for MIDI, used by music backends but also by miditest
    base_libraries += ['winmm', 'psapi']
    common_libraries += [libfbgfx]
    commonenv['FBFLAGS'] += ['-s','gui']
    commonenv['CXXLINKFLAGS'] += ['-lgdi32', '-Wl,--subsystem,windows']
elif mac:
    base_modules += ['os_unix.c', 'os_unix2.bas']
    common_modules += ['os_unix_wm.c']
    common_libraries += ['Cocoa']  # For CoreServices
    if 'sdl' in gfx:
        common_modules += ['mac/SDLmain.m']
        commonenv['FBFLAGS'] += ['-entry', 'SDL_main']
        if env.WhereIs('sdl-config'):
            commonenv.ParseConfig('sdl-config --cflags')
        else:
            commonenv['CFLAGS'] += ["-I", "/Library/Frameworks/SDL.framework/Headers", "-I", FRAMEWORKS_PATH + "/SDL.framework/Headers"]
elif android:
    # liblog for __android_log_print/write
    base_libraries += ['log']
    base_modules += ['os_unix.c', 'os_unix2.bas']
    common_modules += ['os_unix_wm.c']
elif unix:  # Linux & BSD
    base_modules += ['os_unix.c', 'os_unix2.bas']
    common_modules += ['os_unix_wm.c']
    if gfx != ['console']:
        # All graphical gfx backends need the X11 libs
        common_libraries += 'X11 Xext Xpm Xrandr Xrender'.split (' ')
    commonenv['FBFLAGS'] += ['-d', 'DATAFILES=\'"' + prefix + '/share/games/ohrrpgce"\'']


################ Add the libraries to env and commonenv

for lib in base_libraries:
    env['CXXLINKFLAGS'] += ['-l' + lib]
    env['FBLINKFLAGS'] += ['-l', lib]

for lib in base_libraries + common_libraries:
    if mac and lib in ('SDL', 'SDL_mixer', 'Cocoa'):
        # Use frameworks rather than normal unix libraries
        commonenv['CXXLINKFLAGS'] += ['-framework', lib]
        commonenv['FBLINKFLAGS'] += ['-Wl', '-framework,' + lib]
    else:
        commonenv['CXXLINKFLAGS'] += ['-l' + lib]
        commonenv['FBLINKFLAGS'] += ['-l', lib]

if win32:
    env['FBLINKFLAGS'] += ['-p', 'win32']
    env['CXXLINKFLAGS'] += ['-L', 'win32']
    common_libpaths += ['win32']

commonenv['CXXLINKFLAGS'] += ['-L' + path for path in common_libpaths]
commonenv['FBLINKFLAGS'] += Flatten ([['-p', v] for v in common_libpaths])


################ Modules

# The following are linked into all executables, except miditest.
base_modules +=   ['util.bas',
                   'blit.c',
                   'base64.c',
                   'unicode.c',
                   'array.c',
                   'miscc.c',
                   'vector.bas']

# Modules shared by the reload utilities, additional to base_modules
reload_modules =  ['reload.bas',
                   'reloadext.bas',
                   'lumpfile.bas']

# The following are built twice, for Game and Custom, so may use #ifdef to change behaviour
# (.bas files only) 
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

# (.bas files only) 
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

# (.bas files only) 
game_modules = ['game',
                'bmod.rbas',
                'bmodsubs',
                'menustuf.rbas',
                'moresubs.rbas',
                'scriptcommands',
                'yetmore2',
                'walkabouts',
                'savegame.rbas',
                'scripting',
                'oldhsinterpreter',
                'purchase.rbas',
                'plankmenu.bas']

# The following are built only once and linked into Game and Custom
common_modules += ['filelayer.cpp',
                   'rasterizer.cpp',
                   'matrixMath.cpp',
                   'gfx_newRenderPlan.cpp']


################ Generate build & version info files before compiling any modules

builddir = Dir('.').abspath + os.path.sep
rootdir = Dir('#').abspath + os.path.sep
verprint (gfx, music, fbc, builddir, rootdir)


################ Generate object file Nodes

# Note that base_objects are not built in commonenv!
base_objects = Flatten([env.Object(a) for a in base_modules])  # concatenate NodeLists
common_objects = base_objects + Flatten ([commonenv.Object(a) for a in common_modules])
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

# Sort RB modules to the front so they get built first, to avoid bottlenecks
gamesrc.sort (key = lambda node: 0 if '.rbas' in node.path else 1)
editsrc.sort (key = lambda node: 0 if '.rbas' in node.path else 1)

# For reload utilities
reload_objects = base_objects + Flatten ([env.Object(a) for a in reload_modules])

# For utiltest
base_objects_without_util = [a for a in base_objects if str(a) != 'util.o']


################ Executable definitions
# Executables are explicitly placed in rootdir, otherwise they would go in build/

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
else:
    gamename = 'ohrrpgce-game'
    editname = 'ohrrpgce-custom'

def env_exe(name, **kwargs):
    ret = env.BASEXE (rootdir + name, **kwargs)
    Alias (name, ret)
    return ret

GAME = gameenv.BASEXE   (rootdir + gamename, source = gamesrc)
CUSTOM = editenv.BASEXE (rootdir + editname, source = editsrc)
GAME = GAME[0]  # first element of NodeList is the executable
CUSTOM = CUSTOM[0]
env_exe ('bam2mid')
env_exe ('miditest')
env_exe ('unlump', source = ['unlump.bas', 'lumpfile.o'] + base_objects)
env_exe ('relump', source = ['relump.bas', 'lumpfile.o'] + base_objects)
env_exe ('dumpohrkey', source = ['dumpohrkey.bas'] + base_objects)
HSPEAK = env.Command (rootdir + 'hspeak', source = ['hspeak.exw', 'hsspiffy.e'], action = 'euc -gcc hspeak.exw -verbose')
RELOADTEST = env_exe ('reloadtest', source = ['reloadtest.bas'] + reload_objects)
x2rsrc = ['xml2reload.bas'] + reload_objects
if win32:
    # Hack around our provided libxml2.a lacking a function. (Was less work than recompiling)
    x2rsrc.append (env.Object('win32/utf8toisolat1.c'))
XML2RELOAD = env_exe ('xml2reload', source = x2rsrc, FBLINKFLAGS = env['FBLINKFLAGS'] + ['-l','xml2'], CXXLINKFLAGS = env['CXXLINKFLAGS'] + ['-lxml2'])
RELOAD2XML = env_exe ('reload2xml', source = ['reload2xml.bas'] + reload_objects)
RELOADUTIL = env_exe ('reloadutil', source = ['reloadutil.bas'] + reload_objects)
RBTEST = env_exe ('rbtest', source = [env.RB('rbtest.rbas'), env.RB('rbtest2.rbas')] + reload_objects)
VECTORTEST = env_exe ('vectortest', source = ['vectortest.bas'] + base_objects)
# Compile util.bas as a main module to utiltest.o to prevent its linkage in other binaries
UTILTEST = env_exe ('utiltest', source = env.BASMAINO('utiltest.o', 'util.bas') + base_objects_without_util)
env_exe ('slice2bas', source = ['slice2bas.bas'] + reload_objects)

Alias ('game', GAME)
Alias ('custom', CUSTOM)
Alias ('reload', [RELOADUTIL, RELOAD2XML, XML2RELOAD, RELOADTEST, RBTEST])

if android_source:
    # This is hacky and ought to be rewritten
    Alias('game', action = android_source_actions (gamesrc, rootdir, rootdir + 'android/tmp'))
    Alias('custom', action = android_source_actions (editsrc, rootdir, rootdir + 'android/tmp'))
    if 'game' not in COMMAND_LINE_TARGETS and 'custom' not in COMMAND_LINE_TARGETS:
        raise Exception("Specify either 'game' or 'custom' as a target with android-source=1")

# building gfx_directx.dll
if win32:
    directx_sources = ['d3d.cpp', 'didf.cpp', 'gfx_directx.cpp', 'joystick.cpp', 'keyboard.cpp',
                       'midsurface.cpp', 'mouse.cpp', 'window.cpp']
    directx_sources = [os.path.join('gfx_directx', f) for f in directx_sources]

    # Create environment for compiling gfx_directx.dll
    w32_env = Environment ()
    w32_env['ENV']['PATH'] = os.environ['PATH']
    if "Include" in os.environ:
        w32_env.Append(CPPPATH = os.environ['Include'])
    if "Lib" in os.environ:
        w32_env.Append(LIBPATH = os.environ['Lib'])
    if 'DXSDK_DIR' in os.environ:
        w32_env.Append(CPPPATH = os.path.join(os.environ['DXSDK_DIR'], 'Include'))
        w32_env.Append(LIBPATH = os.path.join(os.environ['DXSDK_DIR'], 'Lib', 'x86'))

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

    w32_env.SharedLibrary (rootdir + 'gfx_directx.dll', source = directx_sources,
                          LIBS = ['user32', 'ole32', 'gdi32'])
    TEST = w32_env.Program (rootdir + 'gfx_directx_test1.exe', source = ['gfx_directx/gfx_directx_test1.cpp'],
                            LIBS = ['user32'])
    Alias ('gfx_directx_test', TEST)


# --log . to smooth out inconsistencies between Windows and Unix
tmp = ''
if 'fb' in gfx:
    # Use gfx_fb because it draws far less frames without speed control for some reason, runs waaaay faster
    tmp = ' --gfx fb'


def Phony(name, source, action):
    node = env.Alias(name, source = source, action = action)
    AlwaysBuild(node)  # Run even if there happens to be a file of the same name
    return node

AUTOTEST = Phony ('autotest_rpg', source = GAME, action =
                  [GAME.abspath + tmp +  ' --log . --runfast testgame/autotest.rpg -z 2',
                   'grep -q "TRACE: TESTS SUCCEEDED" g_debug.txt'])
INTERTEST = Phony ('interactivetest', source = GAME, action =
                   [GAME.abspath + tmp + ' --log . --runfast testgame/interactivetest.rpg -z 2'
                    ' --replayinput testgame/interactivetest.ohrkey',
                    'grep -q "TRACE: TESTS SUCCEEDED" g_debug.txt'])
# This prevents more than one copy of Game from being run at once
# (doesn't matter where g_debug.txt is actually placed).
# The Alias prevents scons . from running the tests.
SideEffect (Alias ('g_debug.txt'), [AUTOTEST, INTERTEST])

# There has to be some better way to do this...
tests = [exe.abspath for exe in Flatten([RELOADTEST, RBTEST, VECTORTEST, UTILTEST])]
TESTS = Phony ('test', source = tests + [AUTOTEST, INTERTEST], action = tests)
Alias ('tests', TESTS)

def install(target, source, env):
    if mac or android or not unix:
        print "The 'install' action is only implemented on Unix systems."
        return 1
    sys.path += ['linux']
    import ohrrpgce
    ohrrpgce.install(destdir, prefix)

Phony ('install', source = [GAME, CUSTOM, HSPEAK], action = install)

Default (GAME)
Default (CUSTOM)

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
  debug=0|1|2|3       Debug level:
                                  -exx |  debug info  | optimisation
                                 ------+--------------+--------------
                       debug=0:    no  | symbols only |    yes   <--Releases
                       debug=1:    no  |     yes      |    yes
                       debug=2:    yes |     yes      |    yes   <--Default
                       debug=3:    yes |     yes      |    no
                       debug=4:    no  |     yes      |    no
                      -exx builds have array, pointer and file error checking
                      (they abort immediately on errors!), and are slow.
  valgrind=1          Recommended when using valgrind (also turns off -exx).
  asan=1              Use AddressSanitizer. Unless overridden with gengcc=0 also
                      disables -exx and uses GCC emitter.
  profile=1           Profiling build for gprof.
  scriptprofile=1     Script profiling build: track time in interpreter.
  asm=1               Produce .asm or .c files in build/ while compiling.
  fbc=PATH            Point to a different version of fbc.
  macsdk=version      Target a previous version of Mac OS X, eg. 10.4
                      You will need the relevant SDK installed, and need to use a
                      copy of FB built against that SDK.
  prefix=PATH         For 'install' action. Default: '/usr'
  destdir=PATH        For 'install' action. Use if you want to install into a staging
                      area, for a package creation tool. Default: ''
  v=1                 Be verbose.

Experimental options:
  gengcc=1            Compile using GCC emitter.
  linkgcc=0           Link using fbc instead of g++ (only works for a few targets)
  android=1           Compile for android. Commandline programs only.
  android-source=1    Used as part of the Android build process for Game/Custom.
  glibc=1             Enable memory_usage function
  arch=64             Create a x86_64 build (options: x86/32, x86_64/64)

The following environmental variables are also important:
  FBFLAGS             Pass more flags to fbc
  fbc                 Override FB compiler
  AS, CC, CXX         Override assembler/compiler. Should be set when crosscompiling
  OHRGFX, OHRMUSIC    Specify default gfx, music backends
  DXSDK_DIR, Lib,
     Include          For compiling gfx_directx.dll
  EUDIR               Needed by Euphoria? (when compiling hspeak)

Targets (executables to build):
  """ + gamename + """ (or game)
  """ + editname + """ (or custom)
  gfx_directx.dll
  unlump
  relump
  hspeak
  reloadtest
  xml2reload
  reload2xml
  reloadutil          To compare two .reload documents, or time load time
  utiltest
  vectortest
  rbtest
  slice2bas
  gfx_directx_test    (Non-automated) gfx_directx.dll test
  dumpohrkey
  bam2mid
  miditest
Other targets/actions:
  install             (Unix only.) Install the OHRRPGCE. Uses prefix and destdir args
                      Installs files into ${destdir}${prefix}/games and ${destdir}${prefix}/share
  reload              Compile all RELOAD utilities.
  autotest_rpg        Runs autotest.rpg. See autotest.py for improved harness.
  interactivetest     Runs interactivetest.rpg with recorded input.
  test (or tests)     Compile and run all automated tests, including autotest.rpg.
  .                   Compile everything (but doesn't run tests)

With no targets specified, compiles game and custom.

Examples:
 Do a debug build of Game and Custom:
  scons
 Specifying graphics and music backends for a debug build of Game:
  scons gfx=sdl+fb music=native game
 Do a 'release' build (same as binary distributions) of everything, using 4 CPU cores:
  scons -j 4 debug=0 .
""")
